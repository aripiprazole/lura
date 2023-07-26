import {
  workspace,
  EventEmitter,
  ExtensionContext,
  window,
  TextDocumentChangeEvent,
} from 'vscode';

import {
  Disposable,
  Executable,
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from 'vscode-languageclient/node';

let client: LanguageClient;

export async function activate(_ctx: ExtensionContext) {
  const traceOutputChannel = window.createOutputChannel(
    'lura language server trace',
  );
  const command = process.env.SERVER_PATH || 'lura-language-server';

  const run: Executable = {
    command,
    options: {
      env: {
        ...process.env,
        // eslint-disable-next-line @typescript-eslint/naming-convention
        RUST_LOG: 'error',
        RUST_BACKTRACE: '1',
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
  const clientOptions: LanguageClientOptions = {
    // Register the server for plain text documents
    documentSelector: [{scheme: 'file', language: 'lura'}],
    synchronize: {
      // Notify the server about file changes to '.clientrc files contained in the workspace
      fileEvents: workspace.createFileSystemWatcher('**/.clientrc'),
    },
    traceOutputChannel,
  };

  // Create the language client and start the client.
  client = new LanguageClient(
    'lura-language-server',
    'lura language server',
    serverOptions,
    clientOptions,
  );
  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}

export function activateInlayHints(ctx: ExtensionContext) {
  const maybeUpdater = {
    hintsProvider: null as Disposable | null,
    updateHintsEventEmitter: new EventEmitter<void>(),

    async onConfigChange() {
      this.dispose();
    },

    onDidChangeTextDocument(_event: TextDocumentChangeEvent) {
      this.updateHintsEventEmitter.fire();
    },

    dispose() {
      this.hintsProvider?.dispose();
      this.hintsProvider = null;
      this.updateHintsEventEmitter.dispose();
    },
  };

  workspace.onDidChangeConfiguration(
    maybeUpdater.onConfigChange,
    maybeUpdater,
    ctx.subscriptions,
  );
  workspace.onDidChangeTextDocument(
    maybeUpdater.onDidChangeTextDocument,
    maybeUpdater,
    ctx.subscriptions,
  );

  maybeUpdater.onConfigChange().catch(console.error);
}
