
SingleDensity <- function(data1, x.string){
  ggplot() + 
    geom_density(data=data1, aes_string(x=x.string, group='factor(label)', fill='factor(label)'), alpha=0.5)
}

DoubleDensity <- function(data1, data2, x.string){
  ggplot() + 
    geom_density(data=data1, aes_string(x=x.string, group='factor(label)', fill='factor(label)'), alpha=0.5) +
    geom_density(data=data2, aes_string(x=x.string), fill='#000000', alpha=0.5)
}

SingleScatter <- function(data1, x.string, y.string){
  ggplot() + 
    geom_point(data=data1, aes_string(x=x.string, y=y.string, color='factor(label)'), alpha=.5, size=1)
}

DoubleScatter <- function(data1, data2, x.string, y.string){
  ggplot() + 
    geom_point(data=data1, aes_string(x=x.string, y=y.string, color='factor(label)'), alpha=.5, size=1) + 
    geom_point(data=data2, aes_string(x=x.string, y=y.string), alpha=.5, size=1, color='black')  
}
