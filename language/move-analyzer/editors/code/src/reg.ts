// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

import type { Context } from './context';
import * as vscode from 'vscode';
import * as fs from 'fs';
import * as path from 'path';
import * as childProcess from 'child_process';

/**
 * A logger for the VS Code extension.
 *
 * Messages that are logged appear in an output channel created below that is dedicated to the
 * extension (or "client"), in the extension user's "Output View." This logger should be used for
 * messages related to VS Code and this extension, as opposed to messages regarding the language
 * server, which appear in a separate output channel.
 **/

class TraverseDirItem {
    path: string;

    is_file: boolean;

    constructor(path: string,
        is_file: boolean) {
        this.path = path;
        this.is_file = is_file;
    }
}


function workSpaceDir(): string | undefined {
    if (vscode.workspace.workspaceFolders !== undefined) {
        if (vscode.workspace.workspaceFolders[0] !== undefined) {
            const f = vscode.workspace.workspaceFolders[0].uri.fsPath;
            return f;
        }
    }
    return undefined;
}

async function serverVersion(context: Readonly<Context>): Promise<void> {
    const version = childProcess.spawnSync(
        context.configuration.serverPath,
        ['--version'],
        { encoding: 'utf8' },
    );
    if (version.stdout) {
        await vscode.window.showInformationMessage(version.stdout);
    } else if (version.error) {
        await vscode.window.showErrorMessage(
            `Could not execute aptos-move-analyzer: ${version.error.message}.`,
        );
    } else {
        await vscode.window.showErrorMessage(
            `A problem occurred when executing '${context.configuration.serverPath}'.`,
        );
    }
}

function traverseDir(dir: any, call_back: (path: TraverseDirItem) => void): void {
    fs.readdirSync(dir).forEach(file => {
        const fullPath = path.join(dir, file);
        if (fs.lstatSync(fullPath).isDirectory()) {
            call_back(new TraverseDirItem(fullPath, false));
            traverseDir(fullPath, call_back);
        } else {
            call_back(new TraverseDirItem(fullPath, true));
        }
    });
}

function get_all_move_toml_dirs(): string[] {
    const working_dir = workSpaceDir();
    if (working_dir === undefined) {
        return [];
    }
    const ret: string[] = [];
    traverseDir(working_dir, (item) => {
        if (item.is_file && item.path.endsWith('Move.toml')) {
            ret.push(item.path);
        }
    });
    return ret;
}

class TerminalManager {
    all: Map<string, vscode.Terminal | undefined>;

    constructor() {
        this.all = new Map();
    }

    alloc(typ: string, new_fun: () => vscode.Terminal): vscode.Terminal {
        const x = this.all.get(typ);
        if (x === undefined || x.exitStatus !== undefined) {
            const x = new_fun();
            this.all.set(typ, x);
            return x;
        }
        return x;
    }
}


class WorkingDir {

    private dir: string | undefined;

    constructor() {
        const working_dir = workSpaceDir();
        if (working_dir === undefined) {
            this.dir = undefined;
        }
        const x = get_all_move_toml_dirs();
        if (x.length === 1) {
            this.dir = working_dir;
        }
        this.dir = undefined;
    }

    // Change the current working dir
    set_dir(Dir: string): void {
        this.dir = Dir;
    }

    // Get the current working dir, if is undefined, return ""
    get_dir(): string {
        if (this.dir !== undefined) {
            return this.dir;
        }
        return '';
    }

    async get_use_input_working_dir(): Promise<string | undefined> {
        return vscode.window.showQuickPick(get_all_move_toml_dirs(),
            {
            }).then((x): string | undefined => {
                if (x === undefined) {
                    return undefined;
                }
                this.dir = path.parse(x).dir;
                return this.dir;
            });
    }

    async get_working_dir(): Promise<string | undefined> {
        if (this.dir !== undefined) {
            return this.dir;
        }
        return this.get_use_input_working_dir();
    }

}
const Reg = {

    /** Regist all the command for aptos framework for main.ts */
    regaptos(context: Readonly<Context>): void {
        /**
         * An extension command that displays the version of the server that this extension
         * interfaces with.
         */
        const aptos_working_dir = new WorkingDir();
        const terminalManager = new TerminalManager();
        const schemaTypes = ['ed25519', 'secp256k1', 'secp256r1'];
        const aptos_move_toml_template = `[package]
        name = "my_first_package"
        version = "0.0.3"

        [addresses]
        my_first_package =  "0x0"
        aptos =  "0x2"
        `;
        const aptos_module_file_template = `
        // Copyright (c) Mysten Labs, Inc.
        // SPDX-License-Identifier: Apache-2.0

        module my_first_package::my_module {
            // Part 1: imports
            use aptos::object::{Self, UID};
            use aptos::transfer;
            use aptos::tx_context::{Self, TxContext};

            // Part 2: struct definitions
            struct Sword has key, store {
                id: UID,
                magic: u64,
                strength: u64,
            }

            struct Forge has key {
                id: UID,
                swords_created: u64,
            }

            // Part 3: module initializer to be executed when this module is published
            fun init(ctx: &mut TxContext) {
                let admin = Forge {
                    id: object::new(ctx),
                    swords_created: 0,
                };
                // transfer the forge object to the module/package publisher
                transfer::transfer(admin, tx_context::sender(ctx));
            }

            // Part 4: accessors required to read the struct attributes
            public fun magic(self: &Sword): u64 {
                self.magic
            }

            public fun strength(self: &Sword): u64 {
                self.strength
            }

            public fun swords_created(self: &Forge): u64 {
                self.swords_created
            }

            // Part 5: entry functions to create and transfer swords
            public entry fun sword_create(forge: &mut Forge, magic: u64, strength: u64, recipient: address,
                                          ctx: &mut TxContext) {
                // create a sword
                let sword = Sword {
                    id: object::new(ctx),
                    magic: magic,
                    strength: strength,
                };
                // transfer the sword
                transfer::transfer(sword, recipient);
                forge.swords_created = forge.swords_created + 1;
            }
        }
        `;

        if (aptos_working_dir.get_dir() !== '') {
            void vscode.window.showInformationMessage('aptos working directory set to ' + aptos_working_dir.get_dir());
        }

        // Register handlers for VS Code commands that the user explicitly issues.
        context.registerCommand('serverVersion', serverVersion);
        // Register test button
        context.registerCommand('aptos.test_ui', (_, ...args) => {
            const cwd = args[0] as string;
            const name = args[1] as string;
            const aptos_test = terminalManager.alloc(cwd + 'aptos.test_ui', () => {
                return vscode.window.createTerminal({
                    cwd: cwd,
                    name: 'aptos test',
                });
            });
            aptos_test.show(true);
            aptos_test.sendText('aptos move test ' + name, true);
            aptos_test.show(false);
        });

        context.registerCommand('aptos.create_project', async () => {

            const dir = await vscode.window.showSaveDialog({
                // There is a long term issue about parse()
                // use "." instead of working dir, detail in https://github.com/microsoft/vscode/issues/173687
                defaultUri: vscode.Uri.parse('.'),
            });

            if (dir === undefined) {
                void vscode.window.showErrorMessage('Please input a directory');
                return;
            }
            const dir2 = dir.fsPath;
            fs.mkdirSync(dir2);
            const project_name = path.parse(dir2).base;
            const replace_name = 'my_first_package';
            fs.writeFileSync(dir2 + '/Move.toml',
                aptos_move_toml_template.toString().replaceAll(replace_name, project_name));
            fs.mkdirSync(dir2 + '/sources');
            fs.writeFileSync(dir2 + '/sources/my_module.move',
                aptos_module_file_template.replaceAll(replace_name, project_name));
        });
        context.registerCommand('aptos.move.new', async () => {
            const working_dir = await aptos_working_dir.get_working_dir();
            if (working_dir === undefined) {
                return;
            }
            const name = await vscode.window.showInputBox({
                title: 'New a project',
                placeHolder: 'Type you project name.',
            });
            if (name === undefined) {
                return;
            }
            const t = terminalManager.alloc('aptos.move.new', (): vscode.Terminal => {
                return vscode.window.createTerminal({
                    name: 'aptos move new',
                });
            });
            t.show(true);
            t.sendText('cd ' + working_dir, true);
            t.sendText('aptos move new ' + name, true);
        });
        context.registerCommand('aptos.move.build', async () => {
            const working_dir = await aptos_working_dir.get_working_dir();
            if (working_dir === undefined) {
                return;
            }
            const t = terminalManager.alloc('aptos.move.build', (): vscode.Terminal => {
                return vscode.window.createTerminal({
                    name: 'aptos move build',
                });
            });
            t.show(true);
            t.sendText('cd ' + working_dir, true);
            t.sendText('aptos move build', true);
        });
        context.registerCommand('aptos.move.coverage', async () => {
            const working_dir = await aptos_working_dir.get_working_dir();
            if (working_dir === undefined) {
                return;
            }
            const t = terminalManager.alloc('aptos.move.coverage', (): vscode.Terminal => {
                return vscode.window.createTerminal({
                    name: 'aptos move coverage',
                });
            });
            t.show(true);
            t.sendText('cd ' + working_dir, true);
            t.sendText('aptos move test --coverage', true);
            t.sendText('aptos move coverage summary', true);
        });
        context.registerCommand('aptos.move.test', async () => {
            const working_dir = await aptos_working_dir.get_working_dir();
            if (working_dir === undefined) {
                return;
            }
            const t = terminalManager.alloc('aptos.move.test', (): vscode.Terminal => {
                return vscode.window.createTerminal({
                    name: 'aptos move test',
                });
            });
            t.show(true);
            t.sendText('cd ' + working_dir, true); t.sendText('cd ' + working_dir, true);
            t.sendText('aptos move test', true);
        });
        context.registerCommand('aptos.move.prove', async () => {
            const working_dir = await aptos_working_dir.get_working_dir();
            if (working_dir === undefined) {
                return;
            }
            const t = terminalManager.alloc('aptos.move.prove', (): vscode.Terminal => {
                return vscode.window.createTerminal({
                    name: 'aptos move prove',
                });
            });
            t.show(true);
            t.sendText('cd ' + working_dir, true);
            t.sendText('aptos move prove', true);
        });
        context.registerCommand('aptos.client.active.address', async () => {
            const working_dir = await aptos_working_dir.get_working_dir();
            if (working_dir === undefined) {
                return;
            }
            const t = terminalManager.alloc('aptos.client.active.address', (): vscode.Terminal => {
                return vscode.window.createTerminal({
                    name: 'aptos client active address',
                });
            });
            t.show(true);
            t.sendText('cd ' + working_dir, true);
            t.sendText('aptos client active-address', true);
        });
        context.registerCommand('aptos.client.active.env', async () => {
            const working_dir = await aptos_working_dir.get_working_dir();
            if (working_dir === undefined) {
                return;
            }
            const t = terminalManager.alloc('aptos.client.active.env', (): vscode.Terminal => {
                return vscode.window.createTerminal({
                    name: 'aptos client active env',
                });
            });
            t.show(true);
            t.sendText('cd ' + working_dir, true);
            t.sendText('aptos client active-env', true);
        });
        context.registerCommand('aptos.client.addresses', async () => {
            const working_dir = await aptos_working_dir.get_working_dir();
            if (working_dir === undefined) {
                return;
            }
            const t = terminalManager.alloc('aptos.client.addresses', (): vscode.Terminal => {
                return vscode.window.createTerminal({
                    name: 'aptos client addresses',
                });
            });
            t.show(true);
            t.sendText('cd ' + working_dir, true);
            t.sendText('aptos client addresses', true);
        });
        context.registerCommand('aptos.client.envs', async () => {
            const working_dir = await aptos_working_dir.get_working_dir();
            if (working_dir === undefined) {
                return;
            }
            const t = terminalManager.alloc('aptos.client.envs', (): vscode.Terminal => {
                return vscode.window.createTerminal({
                    name: 'aptos client envs',
                });
            });
            t.show(true);
            t.sendText('cd ' + working_dir, true);
            t.sendText('aptos client envs', true);
        });
        context.registerCommand('aptos.client.gas', async () => {
            const working_dir = await aptos_working_dir.get_working_dir();
            if (working_dir === undefined) {
                return;
            }
            const t = terminalManager.alloc('aptos.client.gas', (): vscode.Terminal => {
                return vscode.window.createTerminal({
                    name: 'aptos client gas',
                });
            });
            t.show(true);
            t.sendText('cd ' + working_dir, true);
            t.sendText('aptos client gas', true);
        });
        context.registerCommand('aptos.client.object', async () => {
            const working_dir = await aptos_working_dir.get_working_dir();
            if (working_dir === undefined) {
                return;
            }
            const objectID = await vscode.window.showInputBox({
                placeHolder: 'Type you object ID.',
            });
            if (objectID === undefined) {
                return;
            }
            const t = terminalManager.alloc('aptos.client.object', (): vscode.Terminal => {
                return vscode.window.createTerminal({
                    name: 'aptos client object',
                });
            });
            t.show(true);
            t.sendText('cd ' + working_dir, true);
            t.sendText('aptos client object ' + objectID, true);
        });
        context.registerCommand('aptos.client.objects', async () => {
            const working_dir = await aptos_working_dir.get_working_dir();
            if (working_dir === undefined) {
                return;
            }
            const t = terminalManager.alloc('aptos.client.objects', (): vscode.Terminal => {
                return vscode.window.createTerminal({
                    name: 'aptos client objects',
                });
            });
            t.show(true);
            t.sendText('cd ' + working_dir, true);
            t.sendText('aptos client objects', true);
        });
        context.registerCommand('aptos.client.publish', async () => {
            const working_dir = await aptos_working_dir.get_working_dir();
            if (working_dir === undefined) {
                return;
            }
            const budget = await vscode.window.showInputBox({
                placeHolder: 'Type you Gas Budget.',
            });
            if (budget === undefined) {
                return;
            }
            const t = terminalManager.alloc('aptos.client.publish', (): vscode.Terminal => {
                return vscode.window.createTerminal({
                    name: 'aptos client publish',
                });
            });
            t.show(true);
            t.sendText('cd ' + working_dir, true);
            t.sendText('aptos client publish --gas-budget ' + budget, true);
        });
        context.registerCommand('aptos.client.new.address', async () => {
            const working_dir = await aptos_working_dir.get_working_dir();
            if (working_dir === undefined) {
                return;
            }
            const schema = await vscode.window.showQuickPick(schemaTypes, {
                canPickMany: false, placeHolder: 'Select you schema.',
            });
            if (schema === undefined) {
                return;
            }
            const t = terminalManager.alloc('aptos.client.new.address', (): vscode.Terminal => {
                return vscode.window.createTerminal({
                    name: 'aptos client new address',
                });
            });
            t.show(true);
            t.sendText('cd ' + working_dir, true);
            t.sendText('aptos client new-address ' + schema, true);
        });
        context.registerCommand('aptos.keytool.generate', async () => {
            const working_dir = await aptos_working_dir.get_working_dir();
            if (working_dir === undefined) {
                return;
            }
            const schema = await vscode.window.showQuickPick(schemaTypes, {
                canPickMany: false, placeHolder: 'Select you schema.',
            });
            if (schema === undefined) {
                return;
            }
            const t = terminalManager.alloc('aptos.client.keytool.generate', (): vscode.Terminal => {
                return vscode.window.createTerminal({
                    name: 'aptos keytool generate',
                });
            });
            t.show(true);
            t.sendText('cd ' + working_dir, true);
            t.sendText('aptos keytool generate ' + schema, true);
        });
        context.registerCommand('aptos.keytool.import', async () => {
            const working_dir = await aptos_working_dir.get_working_dir();
            if (working_dir === undefined) {
                return;
            }
            const m = await vscode.window.showInputBox({
                placeHolder: 'Type your mnemonic phrase.',
            });
            if (m === undefined) {
                return;
            }
            const schema = await vscode.window.showQuickPick(schemaTypes, {
                canPickMany: false, placeHolder: 'Select you schema.',
            });
            if (schema === undefined) {
                return;
            }
            const t = terminalManager.alloc('aptos.client.keytool.import', (): vscode.Terminal => {
                return vscode.window.createTerminal({
                    name: 'aptos keytool import',
                });
            });
            t.show(true);
            t.sendText('cd ' + working_dir, true);
            t.sendText('aptos keytool import ' + m + ' ' + schema, true);
        });
        context.registerCommand('aptos.keytool.list', async () => {
            const working_dir = await aptos_working_dir.get_working_dir();
            if (working_dir === undefined) {
                return;
            }
            const t = terminalManager.alloc('aptos.client.keytool.list', (): vscode.Terminal => {
                return vscode.window.createTerminal({
                    name: 'aptos keytool list',
                });
            });
            t.show(true);
            t.sendText('cd ' + working_dir, true);
            t.sendText('aptos keytool list ', true);
        });
        context.registerCommand('aptos.keytool.load.keypair', async () => {
            const working_dir = await aptos_working_dir.get_working_dir();
            if (working_dir === undefined) {
                return;
            }
            const file = await vscode.window.showOpenDialog({
                canSelectFiles: false,
            });
            if (file === undefined) {
                return;
            }
            if (file.length === 0) {
                return;
            }
            if (file[0] !== undefined) {
                const t = terminalManager.alloc('aptos.client.keytool.load.keypair', (): vscode.Terminal => {
                    return vscode.window.createTerminal({
                        name: 'aptos keytool load keypair',
                    });
                });
                t.show(true);
                t.sendText('cd ' + working_dir, true);
                t.sendText('aptos keytool load-keypair ' + file[0].fsPath, true);
            }
        });
        context.registerCommand('aptos.keytool.show', async () => {
            const working_dir = await aptos_working_dir.get_working_dir();
            if (working_dir === undefined) {
                return;
            }
            const file = await vscode.window.showOpenDialog({
                canSelectFiles: false,
            });
            if (file === undefined) {
                return;
            }
            if (file.length === 0) {
                return;
            }
            if (file[0] !== undefined) {
                const t = terminalManager.alloc('aptos.client.keytool.show', (): vscode.Terminal => {
                    return vscode.window.createTerminal({
                        name: 'aptos keytool show',
                    });
                });
                t.show(true);
                t.sendText('cd ' + working_dir, true);
                t.sendText('aptos keytool show ' + file[0].fsPath, true);
            }
        });
        context.registerCommand('aptos.keytool.unpack', async () => {
            const working_dir = await aptos_working_dir.get_working_dir();
            if (working_dir === undefined) {
                return;
            }
            const str = await vscode.window.showInputBox({
                placeHolder: 'Type your ????',
            });
            if (str === undefined) {
                return;
            }
            const t = terminalManager.alloc('aptos.client.keytool.unpack', (): vscode.Terminal => {
                return vscode.window.createTerminal({
                    name: 'aptos keytool unpack',
                });
            });
            t.show(true);
            t.sendText('cd ' + working_dir, true);
            t.sendText('aptos keytool unpack \'' + str + '\'', true);
        });
        context.registerCommand('aptos.reset.working.space', async () => {
            const new_ = await aptos_working_dir.get_use_input_working_dir();
            if (new_ === undefined) {
                return;
            }
            aptos_working_dir.set_dir(new_);
            void vscode.window.showInformationMessage('aptos working directory set to ' + new_);
        });
    },

};

export { Reg, WorkingDir };
