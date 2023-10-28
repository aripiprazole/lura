<script lang="ts">
  import {onMount} from 'svelte';
  import Parser, {Language, type Tree} from 'web-tree-sitter';

  let code: string;
  let tree: string = 'nothing';
  let parser: Parser;
  let language: Language;

  async function onChange() {
    tree = parser.parse(code).rootNode.toString().replaceAll('\n', '<br>');
  }

  onMount(async () => {
    await Parser.init({
      wasmPath: 'https://raw.githubusercontent.com/aripiprazole/tree-sitter-lura/main/tree-sitter-lura.wasm',
    });
    language = await Parser.Language.load(
      'https://raw.githubusercontent.com/aripiprazole/tree-sitter-lura/main/tree-sitter-lura.wasm',
    );
    parser = new Parser();
    parser.setLanguage(language);
  });
</script>

<article>
  <textarea class="code" bind:value={code} on:change={onChange} />

  <p>{tree}</p>
</article>

<style>
  article {
    display: flex;
    gap: 1rem;
    border-radius: 1rem;
  }

  textarea {
    border-top-left-radius: 1rem;
    border-bottom-left-radius: 1rem;
    outline: none;
    border: 1px solid #fefefe;
    background: #000;
    height: 15rem;
    padding: 1rem;
  }

  p {
    border-top-right-radius: 1rem;
    border-bottom-right-radius: 1rem;
    border: 1px solid #fefefe;
    background: #000;
    color: #fefefe;
    padding: 1rem;
    margin: 0;
    flex: 1;
    text-align: left;
    word-wrap: break-word;
  }
</style>
