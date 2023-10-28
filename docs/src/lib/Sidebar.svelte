<nav>
  <article>
    <h3>Tutorials</h3>
    <ul>
      <li><a href="hello_world">Hello world</a></li>
    </ul>
  </article>
  <article>
    <h3>Features</h3>
    <ul>
      <li><a href="functions">Functions</a></li>
      <li><a href="classes">Classes</a></li>
      <li><a href="strong_typing">Strong typing</a></li>
      <li><a href="type_driven_development">Type-driven development</a></li>
    </ul>
  </article>
  <article>
    <h3>Software foundations</h3>
    <ul>
      <li><a href="dependent-typing">Formal proofs</a></li>
    </ul>
  </article>
</nav>

<style>
nav {
  min-width: 21.25rem;
  display: flex;
  flex-direction: column;
  gap: 2rem;
  /* max-width: 21.25rem;
  max-height: 21.25rem;
  min-width: 20.625rem;
  min-height: 20.625rem; */
}

nav ul {
  list-style: none;
  display: flex;
  flex-direction: column;
  gap: 1rem;
}

nav li a {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  text-decoration: none;
  color: #fefefe;
  font-size: 1rem;
  transition: 0.2s;
}

nav li a::before {
  content: '~';
  margin-right: .5rem;
}

nav li a:hover {
  filter: brightness(0.8);
}

nav article {
  display: flex;
  flex-direction: column;
  gap: 1rem;
}
</style>
