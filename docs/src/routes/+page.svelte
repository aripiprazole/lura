<div class="container">
  <main class="main">
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
    <section class="content">
      <h1>Documentation</h1>
      <p>
        Lorem ipsum dolor, sit amet consectetur adipisicing elit. Iure, quos hic
        corrupti sapiente accusamus delectus tempora? Corporis quam eligendi
        odit! Atque delectus soluta totam qui! Porro velit alias temporibus
        veritatis. Lorem ipsum dolor, sit amet consectetur adipisicing elit.
        Dolor, quasi aut aliquid quisquam earum expedita iste reiciendis impedit
        magnam eius facilis perferendis exercitationem, omnis iure in enim
        laborum eaque nobis! Lorem ipsum dolor sit amet consectetur adipisicing
        elit. Quis, quae? Esse molestias officia nulla laudantium obcaecati cum
        sequi atque nam assumenda laboriosam rerum aliquid, consequuntur ipsam
        rem? Dolores, doloribus! Tenetur. Lorem ipsum dolor, sit amet
        consectetur adipisicing elit. Laborum, molestias error explicabo
        laudantium, ipsam odit inventore nemo totam quibusdam vitae officiis
        officia et cupiditate assumenda. Ipsum accusantium ab iure commodi?
      </p>
      <article>
        <h2>Simplicity</h2>
        <p>
          Lorem ipsum dolor, sit amet consectetur adipisicing elit. Iure, quos
          hic corrupti sapiente accusamus delectus tempora? Corporis quam
          eligendi odit! Atque delectus soluta totam qui! Porro velit alias
          temporibus veritatis. Lorem ipsum dolor, sit amet consectetur
          adipisicing elit. Dolor, quasi aut aliquid quisquam earum expedita
          iste reiciendis impedit magnam eius facilis perferendis
          exercitationem, omnis iure in enim laborum eaque nobis! Lorem ipsum
          dolor sit amet consectetur adipisicing elit. Quis, quae? Esse
          molestias officia nulla laudantium obcaecati cum sequi atque nam
          assumenda laboriosam rerum aliquid, consequuntur ipsam rem? Dolores,
          doloribus! Tenetur. Lorem ipsum dolor, sit amet consectetur
          adipisicing elit. Laborum, molestias error explicabo laudantium, ipsam
          odit inventore nemo totam quibusdam vitae officiis officia et
          cupiditate assumenda. Ipsum accusantium ab iure commodi?
        </p>
      </article>
      <article>
        <h2>Functional</h2>
        <p>
          Lorem ipsum dolor, sit amet consectetur adipisicing elit. Iure, quos
          hic corrupti sapiente accusamus delectus tempora? Corporis quam
          eligendi odit! Atque delectus soluta totam qui! Porro velit alias
          temporibus veritatis. Lorem ipsum dolor, sit amet consectetur
          adipisicing elit. Dolor, quasi aut aliquid quisquam earum expedita
          iste reiciendis impedit magnam eius facilis perferendis
          exercitationem, omnis iure in enim laborum eaque nobis! Lorem ipsum
          dolor sit amet consectetur adipisicing elit. Quis, quae? Esse
          molestias officia nulla laudantium obcaecati cum sequi atque nam
          assumenda laboriosam rerum aliquid, consequuntur ipsam rem? Dolores,
          doloribus! Tenetur. Lorem ipsum dolor, sit amet consectetur
          adipisicing elit. Laborum, molestias error explicabo laudantium, ipsam
          odit inventore nemo totam quibusdam vitae officiis officia et
          cupiditate assumenda. Ipsum accusantium ab iure commodi?
        </p>
      </article>
      <article>
        <h2>Ergonomy</h2>
        <p>
          Lorem ipsum dolor, sit amet consectetur adipisicing elit. Iure, quos
          hic corrupti sapiente accusamus delectus tempora? Corporis quam
          eligendi odit! Atque delectus soluta totam qui! Porro velit alias
          temporibus veritatis. Lorem ipsum dolor, sit amet consectetur
          adipisicing elit. Dolor, quasi aut aliquid quisquam earum expedita
          iste reiciendis impedit magnam eius facilis perferendis
          exercitationem, omnis iure in enim laborum eaque nobis! Lorem ipsum
          dolor sit amet consectetur adipisicing elit. Quis, quae? Esse
          molestias officia nulla laudantium obcaecati cum sequi atque nam
          assumenda laboriosam rerum aliquid, consequuntur ipsam rem? Dolores,
          doloribus! Tenetur. Lorem ipsum dolor, sit amet consectetur
          adipisicing elit. Laborum, molestias error explicabo laudantium, ipsam
          odit inventore nemo totam quibusdam vitae officiis officia et
          cupiditate assumenda. Ipsum accusantium ab iure commodi?
        </p>
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
}

.content > article {
  display: flex;
  flex-direction: column;
  gap: 1rem;
}

.main > nav {
  min-width: 21.25rem;
  /* max-width: 21.25rem;
  max-height: 21.25rem;
  min-width: 20.625rem;
  min-height: 20.625rem; */
}

.main > nav ul {
  list-style: none;
  display: flex;
  flex-direction: column;
  gap: 1rem;
}

.main > nav li a {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  text-decoration: none;
  color: #fefefe;
  font-size: 1rem;
  transition: 0.2s;
}

.main > nav li a::before {
  content: '~';
  margin-right: .5rem;
}
.main > nav li a:hover {
  filter: brightness(0.8);
}

.main > nav {
  display: flex;
  flex-direction: column;
  gap: 2rem;
}

.main > nav article {
  display: flex;
  flex-direction: column;
  gap: 1rem;
}
</style>
