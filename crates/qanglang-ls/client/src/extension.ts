/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import {
  EventEmitter,
  ExtensionContext,
  TextDocumentChangeEvent,
  window,
  workspace,
  commands,
} from "vscode";

import {
  Disposable,
  Executable,
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient;
// type a = Parameters<>;

// Get the server path from settings
const config = workspace.getConfiguration("qls-language-server");
const serverPath = config.get<string>("serverPath") || "qang ls";

// Split the command and arguments properly
const parts = serverPath.split(" ");
const command = parts[0];
const args = parts.slice(1);

export async function activate(context: ExtensionContext) {
  const traceOutputChannel = window.createOutputChannel(
    "QangLang Language Server trace"
  );
  const run: Executable = {
    command,
    args,
    options: {
      env: {
        ...process.env,
        // eslint-disable-next-line @typescript-eslint/naming-convention
        RUST_LOG: "debug",
      },
    },
  };
  const serverOptions: ServerOptions = {
    run,
    debug: run,
  };
  // If the extension is launched in debug mode then the debug server options are used
  // Otherwise the run options are used
  // Options to control the language client
  let clientOptions: LanguageClientOptions = {
    // Register the server for plain text documents
    documentSelector: [{ scheme: "file", language: "qanglang" }],
    synchronize: {
      // Notify the server about file changes to .ql files contained in the workspace
      fileEvents: workspace.createFileSystemWatcher("**/*.ql"),
    },
    traceOutputChannel,
  };

  // Add restart command
  const restartCommand = commands.registerCommand(
    "qanglang.restartServer",
    async () => {
      if (client) {
        window.showInformationMessage("Restarting QangLang Language Server...");
        await client.stop();
        client.start();
        window.showInformationMessage("QangLang Language Server restarted!");
      }
    }
  );

  context.subscriptions.push(restartCommand);

  // Create the language client and start the client.
  client = new LanguageClient(
    "qls-language-server",
    "QangLang Language Server",
    serverOptions,
    clientOptions
  );
  // activateInlayHints(context);
  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}

// export function activateInlayHints(ctx: ExtensionContext) {
//   const maybeUpdater = {
//     hintsProvider: null as Disposable | null,
//     updateHintsEventEmitter: new EventEmitter<void>(),

//     async onConfigChange() {
//       this.dispose();

//       const event = this.updateHintsEventEmitter.event;
// this.hintsProvider = languages.registerInlayHintsProvider(
//   { scheme: "file", language: "nrs" },
//   // new (class implements InlayHintsProvider {
//   //   onDidChangeInlayHints = event;
//   //   resolveInlayHint(hint: InlayHint, token: CancellationToken): ProviderResult<InlayHint> {
//   //     const ret = {
//   //       label: hint.label,
//   //       ...hint,
//   //     };
//   //     return ret;
//   //   }
//   //   async provideInlayHints(
//   //     document: TextDocument,
//   //     range: Range,
//   //     token: CancellationToken
//   //   ): Promise<InlayHint[]> {
//   //     const hints = (await client
//   //       .sendRequest("custom/inlay_hint", { path: document.uri.toString() })
//   //       .catch(err => null)) as [number, number, string][];
//   //     if (hints == null) {
//   //       return [];
//   //     } else {
//   //       return hints.map(item => {
//   //         const [start, end, label] = item;
//   //         let startPosition = document.positionAt(start);
//   //         let endPosition = document.positionAt(end);
//   //         return {
//   //           position: endPosition,
//   //           paddingLeft: true,
//   //           label: [
//   //             {
//   //               value: `${label}`,
//   //               // location: {
//   //               //   uri: document.uri,
//   //               //   range: new Range(1, 0, 1, 0)
//   //               // }
//   //               command: {
//   //                 title: "hello world",
//   //                 command: "helloworld.helloWorld",
//   //                 arguments: [document.uri],
//   //               },
//   //             },
//   //           ],
//   //         };
//   //       });
//   //     }
//   //   }
//   // })()
// );
//   },

//   onDidChangeTextDocument({
//     contentChanges,
//     document,
//   }: TextDocumentChangeEvent) {
//     // debugger
//     // this.updateHintsEventEmitter.fire();
//   },

//   dispose() {
//     this.hintsProvider?.dispose();
//     this.hintsProvider = null;
//     this.updateHintsEventEmitter.dispose();
//   },
// };

// workspace.onDidChangeConfiguration(
//   maybeUpdater.onConfigChange,
//   maybeUpdater,
//   ctx.subscriptions
// );
// workspace.onDidChangeTextDocument(
//   maybeUpdater.onDidChangeTextDocument,
//   maybeUpdater,
//   ctx.subscriptions
// );

// maybeUpdater.onConfigChange().catch(console.error);
// }
