// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

import * as os from 'os';
import * as vscode from 'vscode';
import * as Path from 'path';

class InlayHintsConfig {
    public enable: boolean;
    constructor(enable: boolean) {
        this.enable = enable;
    }
}

class FmtConfig {
    enable: boolean;
    max_width: number;
    indent_size: number;

    constructor(
        enable: boolean,
        max_width: number,
        indent_size: number) {
        this.enable = enable;
        this.max_width = max_width;
        this.indent_size = indent_size;
    }
}

/**
 * User-defined configuration values, such as those specified in VS Code settings.
 *
 * This provides a more strongly typed interface to the configuration values specified in this
 * extension's `package.json`, under the key `"contributes.configuration.properties"`.
 */


class Configuration {
    private readonly configuration: vscode.WorkspaceConfiguration;

    constructor() {
        this.configuration = vscode.workspace.getConfiguration('aptos-move-analyzer');
    }

    /** A string representation of the configured values, for logging purposes. */
    toString(): string {
        return JSON.stringify(this.configuration);
    }

    /** The path to the aptos-move-analyzer executable. */
    get serverPath(): string {
        const defaultName = 'aptos-move-analyzer';
        let serverPath = this.configuration.get<string>('server.path', defaultName);
        if (serverPath.length === 0) {
            // The default value of the `server.path` setting is 'aptos-move-analyzer'.
            // A user may have over-written this default with an empty string value, ''.
            // An empty string cannot be an executable name, so instead use the default.
            return defaultName;
        }

        if (serverPath === defaultName) {
            // If the program set by the user is through PATH,
            // it will return directly if specified
            return defaultName;
        }

        if (serverPath.startsWith('~/')) {
            serverPath = os.homedir() + serverPath.slice('~'.length);
        }

        if (process.platform === 'win32' && !serverPath.endsWith('.exe')) {
            serverPath = serverPath + '.exe';
        }
        return Path.resolve(serverPath);
    }

    inlay_hints_config(): InlayHintsConfig {
        const enable = this.configuration.get<boolean>('inlay.hints.enable')!;
        return new InlayHintsConfig(enable);
    }

    movefmt_config(): FmtConfig {
        const enable = this.configuration.get<boolean>('movefmt.enable')!;
        const max_width = this.configuration.get<number>('movefmt.max_width')!;
        const indent_size = this.configuration.get<number>('movefmt.indent_size')!;
        return new FmtConfig(enable, max_width, indent_size);
    }
}

export { InlayHintsConfig, FmtConfig, Configuration };
