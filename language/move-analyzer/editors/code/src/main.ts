// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

import { Configuration } from './configuration';
import { Context } from './context';
import { Extension } from './extension';
import { log } from './log';
import { Reg } from './reg';
import * as vscode from 'vscode';
// import * as lc from "vscode-languageclient/node";

// class ExperimentalFeatures implements lc.StaticFeature {
//   fillInitializeParams?: (params: lc.InitializeParams) => void;
//   preInitialize?: (capabilities: lc.ServerCapabilities<any>, documentSelector: lc.DocumentSelector | undefined) => void;
//   clear(): void {
//     throw new Error('Method not implemented.');
//   }
//   getState(): lc.FeatureState {
//       return { kind: "static" };
//   }

//   fillClientCapabilities(capabilities: lc.ClientCapabilities): void {
//       capabilities.workspace = {
//         inlayHint: {
//           refreshSupport: true
//         }
//       };
//       capabilities.textDocument = {
//         formatting: {
//           dynamicRegistration: true
//         },
//         inlayHint: {
//           dynamicRegistration: true,
//           resolveSupport: {
//             properties: []
//           },
//         }
//       };
//       capabilities.experimental = {
//           snippetTextEdit: true,
//           codeActionGroup: true,
//           hoverActions: true,
//           serverStatusNotification: true,
//           colorDiagnosticOutput: true,
//           openServerLogs: true,
//           commands: {
//               commands: [
//                   "editor.action.triggerParameterHints",
//               ],
//           },
//           ...capabilities.experimental,
//       };
//   }
//   initialize(
//       _capabilities: lc.ServerCapabilities,
//       _documentSelector: lc.DocumentSelector | undefined,
//   ): void {}
//   dispose(): void {}
// }

export async function activate(
  extensionContext: Readonly<vscode.ExtensionContext>,
): Promise<void> {
  const extension = new Extension();
  log.info(`${extension.identifier} version ${extension.version}`);

  const configuration = new Configuration();
  log.info(`configuration: ${configuration.toString()}`);

  const context = Context.create(extensionContext, configuration);
  // An error here -- for example, if the path to the `aptos-move-analyzer` binary that the user
  // specified in their settings is not valid -- prevents the extension from providing any
  // more utility, so return early.
  if (context instanceof Error) {
    void vscode.window.showErrorMessage(
      `Could not activate aptos-move-analyzer: ${context.message}.`,
    );
    return;
  }

  // const d = vscode.languages.registerInlayHintsProvider(
  //   { scheme: 'file', language: 'move' },
  //   {
  //     provideInlayHints(document, range) {
  //       const client = context.getClient();
  //       if (client === undefined) {
  //         return undefined;
  //       }
  //       const hints = client.sendRequest<vscode.InlayHint[]>('textDocument/inlayHint',
  //         { range: range, textDocument: { uri: document.uri.toString() } });
  //       return hints;
  //     },
  //   },
  // );


  // Configure other language features.
  context.configureLanguage();

  // All other utilities provided by this extension occur via the language server.
  // await context.startClient();
  context.startClient();

  // Regist all the aptos commands.
  Reg.regaptos(context);
  // {
  //   const client = context.getClient();
  //   if (client != undefined) {
  //     log.info("registerFeature ExperimentalFeatures");
  //     client.registerFeature(new ExperimentalFeatures());
  //   }
  // }
  // extensionContext.subscriptions.push(d);

  const reload_cfg = function(): any {
    const client = context.getClient();
    if (client !== undefined) {
      const new_configuration = new Configuration();
      log.info(`new_configuration: ${new_configuration.toString()}`);
      void client.sendRequest('move/lsp/client/inlay_hints/config', new_configuration.inlay_hints_config());
      void client.sendRequest('move/lsp/movefmt/config', new_configuration.movefmt_config());
    }
  };
  reload_cfg();
  vscode.workspace.onDidChangeConfiguration(() => {
    log.info('reload_cfg ...  ');
    reload_cfg();
  });
}
