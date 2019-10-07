tabela = function(){
   $('tr:nth-child(10) ~ tr:not(#tabexp)').addClass('escondido');
   $('tr#tabexp').on('click', function(){
      $('tr:nth-child(10) ~ tr:not(#tabexp)').toggleClass('escondido');
      $('tr#tabexp td').toggleClass('menos');
   });
}

$(document).on("shiny:value", function(e) {
   if (e.name == "tabela") {  // mytable is the name / id of the output element
      e.preventDefault();
      $("#tabela").html(e.value);  // render the output from the server
      if(e.value.match(/<tr>/g || []).length > 12)
         tabela();
   }
});