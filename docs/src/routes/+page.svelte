<script>
  import Sidebar from "$lib/Sidebar.svelte";
  import Code from "$lib/Code.svelte";
</script>

<svelte:head>
  <title>Lura</title>
</svelte:head>

<div class="container">
  <main class="main">
    <Sidebar />
    <section class="content">
      <h1>Welcome to Lura! 👋</h1>
      <p>
        Lura is an experimental project that aims to bring to the user, the
        power of a functional language, with powerful and expressive types and
        tooling. It's focused on an incremental architecture, to be able to
        optimize and compile with the best performance possible.
      </p>
      <article>
        <h2>Query-based architecture 🪄</h2>
        <p>
          The compiler is focused on a query-based architecture, where the
          compiler will only compile the code that is needed, and will only
          recompile the code that is affected by the changes. This will allow
          the compiler to be fast and efficient, and will allow powerful and
          easy-to-use tooling in the common IDEs.
        </p>
      </article>
      <article>
        <h2>Playground 🦦</h2>
        <p>
          You can play with the Lura's parse tree, that is written with
          tree-sitter and rendered with the tree-sitter web package. In the near
          future you will be able to play with the compiler and the language
          itself.
        </p>
        <Code />
      </article>
    </section>
  </main>
</div>

<style>
.container {
  display: flex;
  width: 100%;
  height: 100%;
  justify-content: center;
  min-width: 25.625rem;
}

.main {
  width: 100%;
  max-width: 80rem;

  display: flex;
  gap: 3rem;

  margin: auto;
  padding: 10rem 2rem;
}

.content {
  display: flex;
  flex-direction: column;
  gap: 3rem;
  max-width: 51rem;
}

.content > article {
  display: flex;
  flex-direction: column;
  gap: 1rem;
}
</style>
