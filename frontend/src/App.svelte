<script>
  import {onMount} from 'svelte'
  import svelteLogo from './assets/svelte.svg'
  import viteLogo from '/vite.svg'
  import Counter from './lib/Counter.svelte'
  import * as d3 from "d3";


  let inv2 = {
    "amt" : 1
  , "bolt11" : ""
  , "status" : "Nada"
  }
 let split = {
      "invoiceid":""
    , "bolt11" : ""
    , "status" : "Nada"
    , "parts" : [inv2, Object.assign({},inv2) ]
 }
 let width = 300 
 $: height = width
 $: radius = (Math.min(width, height) / 2) - 2
 
 
 let colStatus = {
    "Nada"    : "#4d4e4d"
  , "Created" : "#377eb8"
  , "Held"    : "#f2a900"
  , "Paid"    : "#10aF10"
 }

  $: data = split.parts 
  $: pie = d3.pie().value(u => u.amt)(data);
  $: arc = d3.arc().innerRadius(width/20) .outerRadius(radius);

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
  function oneMore () {
    split.parts.push(Object.assign({},inv2))
    split.parts = split.parts
  }

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
  <h1 > --- split invoice --- </h1>
  {#if split.status=="Nada"}
  <div> Enter Invoice to Split: </div>
  <div> <input bind:value={split.bolt11}> </div> 
  {:else} 
    <p> {split.bolt11} </p>
  {/if}
  <svg id="splitchart" {width} {height}> 
    {#each pie as p}
        <g class="arc" transform="translate({width/2},{height/2})">  
            <path d={arc(p)}} fill={colStatus[p.data.status]}> </path>
        </g>
    {/each}
    <circle fill={colStatus[split.status]} transform="translate({width/2},{height/2})" r="33"></circle>
  </svg>
  <h4> Split By {split.parts.length} <span on:click={oneMore}> + </span> </h4>
  {#each split.parts as m}
     <div>
        {#if m.bolt11} 
		<p> {m.bolt11} </p>
        {:else}
        <input type=number bind:value={m.amt}>  
        {/if}
    </div>
  {/each}
  <button on:click={doPost}> Create Split </button>  
    
</main>

<style>
  g path, circle { 
    stroke: white;
    stroke-width: 4;
  }
    
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
