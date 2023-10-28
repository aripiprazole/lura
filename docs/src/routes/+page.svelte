<script>
  import Sidebar from "$lib/Sidebar.svelte";
</script>

<div class="container">
  <main class="main">
    <Sidebar />
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
</style>
