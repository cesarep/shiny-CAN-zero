X = x0
FX = f(x0)
DX = dfdx(x0)
FXDX = FX/DX

i = 1
while(abs(FX[i]) > prec){
   if(!is.finite(X[i]-DX[i])){ #checa se vai gerar um valor real
      status = list(s = FALSE, msg = "Valor não real produzido")
      break
   }
   
   X[i+1] = X[i] - FXDX[i]
   FX[i+1] = f(X[i+1])
   DX[i+1] = dfdx(X[i+1])
   FXDX[i+1] = FX[i+1]/DX[i+1]
   
   i = i+1
   if(i > 50){
      status = list(s = FALSE, msg = "50 iterações excedido")
      break
   }
   status = list(s = TRUE, xf = X[i], fxf= FX[i], cab="<tr>
	<th>\\(i\\)</th>
	<th>\\(x_i\\)</th>
	<th>\\(f(x_i)\\)</th>
	<th>\\(f'(x_i)\\)</th>
	<th>\\(f(x_i)/f'(x_i)\\)</th>
</tr>")
}

tabela <- cbind(i = 1:i, X, FX, DX, FXDX)