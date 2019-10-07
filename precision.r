precisionInput <- function(inputId, label, value = 0.05){
	tagList(
		shiny::singleton(shiny::tags$head(tags$script(HTML(precisionJS)),
													 shiny::tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/decimal.js/10.2.0/decimal.min.js")
		)
		),
		shiny::tags$div(class='shiny-input-container form-group precision-input',
							 shiny::tags$label('for' = inputId, label),
							 shiny::tags$div(class='input-group',
							 					 shiny::tags$span(class='input-group-addon btn btn-default', 'data-opr'=1,'for'=inputId, shiny::HTML("&#9650;")),
							 					 shiny::tags$input(class="form-control", id=inputId, type='text', value=gsub("\\.", ",", c(as.character(value))) ),
							 					 shiny::tags$span(class='input-group-addon btn btn-default', 'data-opr'=-1, 'for'=inputId, shiny::HTML("&#9660;"))
					)
		)
	)
}

precisionJS <- "var precisionBinding = new Shiny.InputBinding();
$.extend(precisionBinding, {
  find: function(scope) {
    return $(scope).find('.precision-input input');
  },
  getValue: function(el) {
    return  parseFloat($(el).val().replace(',', '.'));
  },
  subscribe: function(el, callback) {
  	 $(el).on('change.precisionBinding', function(e) {
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off('.precisionBinding');
  }
});

Shiny.inputBindings.register(precisionBinding);

mtply = function(input, opr){
	let val = new Decimal(parseFloat(input.val().replace(',', '.')))
	let newVal = val.mul(Decimal.pow(10, opr)).toPrecision()
	input.val(newVal.replace('.', ','))
}

$(document).on('click', '.precision-input span', function(evt) {
  let input = $('#' + $(evt.target).attr('for'));
  let opr = $(evt.target).attr('data-opr');
  mtply(input, opr)
  input.trigger('change');
});

$(document).on('keydown wheel', '.precision-input input', function(evt) {
   let input = $(evt.target);
  	let delta = evt.originalEvent.deltaY
	if (evt.keyCode == 38 | evt.keyCode == 40) {
	  	mtply(input, (evt.keyCode == 38 ? 1 : -1))
	  	input.trigger('change');
	} else if (isFinite(delta) && delta !== 0) {
		evt.preventDefault();
	  	mtply(input, delta > 1 ? -1 : 1)
	  	input.trigger('change');
	}
});

$(document).on('input', '.precision-input input', function(evt){
	let input = $(evt.target)
	input.val(input.val().replace(/[^0-9.,]+/,''))
})
"