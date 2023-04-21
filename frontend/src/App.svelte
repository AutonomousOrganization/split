<script>
  import {onMount} from 'svelte'
  import svelteLogo from './assets/svelte.svg'
  import viteLogo from '/vite.svg'
  import Counter from './lib/Counter.svelte'
  import * as d3 from "d3";

 let width = 450
 let height = 450
 // let margin = 40
 // let radius = Math.min(width, height) / 2 - margin
 // let parts = 5 
 // let invoice = []
 // let selected = 0
 // 
 // let pie = d3.pie()
 // let data = [1, 1, 2, 3, 5, 8, 13, 21];
 // let svg = d3.select("#splitchart")
 // let g = svg.append("g")
 //            .attr("transform", "translate(" + width / 2 + "," + height / 2 + ")")
 // let arc = d3.arc().innerRadius(0).outerRadius(radius)
 // let arcs = g.selectAll("g").data(pie(data)).enter().append("g").attr("class","arc")

 // arcs.append("path").attr("fill", "#336699" ).attr("d",arc) 
 onMount(()=>{
    var data = [2, 4, 8, 10];

    var svg = d3.select("#splitchart"),
        radius = Math.min(width, height) / 2,
        g = svg.append("g").attr("transform", "translate(" + width / 2 + "," + height / 2 + ")");

    var color = d3.scaleOrdinal(['#4daf4a','#377eb8','#ff7f00','#984ea3','#e41a1c']);

    // Generate the pie
    var pie = d3.pie()(data);
    // Generate the arcs
    var arc = d3.arc()
                .innerRadius(0)
                .outerRadius(radius);

    console.log({pie,arc})
    //Generate groups
        
    console.log(pie) 
    var arcs = g.selectAll("arc")
                .data(pie)
                .enter()
                .append("g")
                .attr("class", "arc")

    //Draw arc paths
    arcs.append("path")
        .attr("fill", function(d, i) {
            return color(i);
        })
        .attr("d", arc);
  });
    


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
  <h1>split invoices</h1>
  <div id="my_dataviz"></div>
  <p> test </p>
  <svg id="splitchart" {width} {height}> 
  
  </svg>

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
