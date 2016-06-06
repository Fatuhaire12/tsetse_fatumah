

# cell[i,j]
fisher.func <- function(cell,cell_iplus1,cell_iminus1,cell_jplus1,cell_jminus1,
                        r=growth.rate,alpha=diff.coef,K=carry.cap){
  growth <- cell * r * (1 - (cell / K))
  diffusion <- cell + alpha * 
    ((cell_iminus1 + cell_jminus1 + cell_iplus1 + cell_jplus1) - (4*cell))
  new_cell_val <- growth + diffusion
  return(new_cell_val)
}
