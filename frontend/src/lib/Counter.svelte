<script>

  let count = 0
  const increment = () => {
    count += 1
  }
    
  const evtSource = new EventSource("/eventfeed") 
  evtSource.addEventListener("ao", evt => { 
    console.log("ao..." , evtdd) 
    increment()
  }) 
  

</script>

<button on:click={increment}>
  count is {count}
</button>
