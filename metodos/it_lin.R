X = x0
GX = g(x0)
FX = f(g(x0))

i = 1
if(is.finite(GX[i])){
   while(is.finite(GX[i]) && abs(GX[i]-X[i]) > prec){
      X[i+1] = g(X[i])
      GX[i+1] = g(X[i+1])
      FX[i+1] = f(g(X[i]))
      i = i+1
      if(i > 50){
         status = list(s = FALSE, msg = "50 iterações excedido")
         break
      }
      if(!is.finite(GX[i])){ #checa se vai gerar um valor real
         status = list(s = FALSE, msg = "Valor não real produzido")
         break
      }
      status = list(s = TRUE, xf = GX[i], fxf = FX[i], cab="<tr>
	<th>\\(i\\)</th>
	<th>\\(x_i\\)</th>
	<th>\\(g(x_i)\\)</th>
	<th>\\(f(g(x_i))\\)</th>
</tr>")
   }
} else {
   status = list(s = FALSE, msg = "Valor não real produzido")
}


tabela <- cbind(i = 1:i, X, GX, FX)