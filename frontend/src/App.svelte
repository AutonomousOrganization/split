<script>
  import svelteLogo from './assets/svelte.svg'
  import viteLogo from '/vite.svg'
  import Counter from './lib/Counter.svelte'

    
  
  async function doPost () {
      const res = await fetch('events', {
  		method: 'POST',
        headers: {
              'Content-Type': 'application/json'
          },
  		body: JSON.stringify({})
  	  })
  }

  const evtSource = new EventSource("/eventfeed") 

  let messages = [420, 421]
  evtSource.addEventListener("message", console.log)     

  evtSource.addEventListener("ao", e => { 
    messages.push(messages.length)
    messages = messages
    console.log("ao..." , e) 
  }) 

  

</script>

<main>

  <div>
    <a href="https://vitejs.dev" target="_blank" rel="noreferrer">
      <img src={viteLogo} class="logo" alt="Vite Logo" />
    </a>
    <a href="https://svelte.dev" target="_blank" rel="noreferrer">
      <img src={svelteLogo} class="logo svelte" alt="Svelte Logo" />
    </a>
  </div>
  <h1>split invoice</h1>
  <ul>
	{#each messages as m}
		<li> x {m}</li>
	{/each}
  </ul>
  <button on:click={doPost}> send value </button>  
    
</main>

<style>
  .logo {
    height: 6em;
    padding: 1.5em;
    will-change: filter;
    transition: filter 300ms;
  }
  .logo:hover {
    filter: drop-shadow(0 0 2em #646cffaa);
  }
  .logo.svelte:hover {
    filter: drop-shadow(0 0 2em #ff3e00aa);
  }
  .read-the-docs {
    color: #888;
  }
</style>
