
# 157: Modelización -------------------------------------------------------

  #* reconocer patrones, señales, ruido, información demás


# Sesión 158: Elección y ajuste del modelo ---------------------------------

  # modelización : resumen del dataset
   # * pelar el data set
   
  # Famila de modelos que expresa el patrón/relación a estudiar

  #  y = a_1 * x + a_0 -> relación lineal
  #  y = a_2 * x^2 + a_1 * x + a_0 -> relacióin cuadrática

 # ajutar el modelo, buscar el parámetro

  # y = 6x - 8
  # y = 2x^2 - 5x + 8

    # * del modelo elegido cuál se ajusta mejor a los datos

  library(modelr)
  
   # cuando trabajamos con datos, podemos fijar una advertencia en caso de NAs
  options(na.action = na.warn)

  # hay varias datset llamadas 'sim'  que se utilizarán en esta sección
  
  

# Sesión 159: Introducción a los modelos lineales -------------------------
# ejemplo  ----------------------------------------------------------------

  sim1 %>%  View()
  
  sim1 %>% 
    ggplot(aes(x,y)) + 
    geom_point()
  
  
  # qué familia de modelo se ajusta mejor
  # y = a_0 + a_1 * x
  
  # generar un conjunto de valores para a0 y a1
  models <- tibble(
    a0 = runif(300, -20, 40),
    a1 = runif(300, -5, 5)
  )
  
  
  # voy a graficar añadiendo al gráfico estos valores
  
  sim1 %>% 
    ggplot(aes(x,y)) + 
    geom_abline(aes(intercept = a0, slope = a1), data = models, alpha = 0.2) +
    geom_point()
  
  model1 <- function(a0, a1, data){
    a0 + data$x * a1
  }
  
  model1(3,1.2, sim1)
  
  
  sim1$y - model1(3,1.2, sim1)
  
  # uso del error cuadrático medio 
  
  rmse <- function(mod, data){
    diff <- data$y - model1(mod[1], mod[2], data)
    sqrt(mean(diff^2))
  }
  
  rmse(c(3, 1.2), sim1)
  
  
  
  sim1_rmse <- function(a0, a1){
    
    rmse(c(a0,a1), sim1)
  }
  
  models <- models %>%
                mutate(rmse = purrr:::map2_dbl(a0,a1, sim1_rmse))
    
  
  
  sim1 %>% 
    ggplot(aes(x, y)) +
    geom_point(size = 2, color = 'grey30') +
    geom_abline(aes(intercept = a0, slope = a1, color = -rmse), 
                data = filter(models, rank(rmse) <= 10))  
  
  models %>% 
    arrange(rmse)
  
   models %>% 
     filter(rank(rmse) <= 5)
  

# Sesión 160 : Cómo encontrar el 'mejor' modelo -----------------------------

   models %>% 
     ggplot(aes(a0,a1)) + 
     # añadimos los aquellos que tienen mejor ajuste
     geom_point(data = filter(models, rank(rmse) <= 10), size = 4, color = 'red') +
     geom_point(aes(color = -rmse)) 
   
   
   
 # formas más sistemática 
   
   grid <- expand.grid(                  # hacer la combinaciones posibles que cubran todo el plano
     a0 = seq(3,5, length= 25),
     a1 =  seq(1.9, 2.2, length = 25)
   ) %>% 
     mutate(rmse = purrr::map2_dbl(a0, a1, sim1_rmse))
   
   
   grid %>% 
     ggplot(aes(a0,a1)) + 
     geom_point(data = filter(grid, rank(rmse) <= 10), size = 4, color = 'red') + 
     geom_point(aes(color = (-rmse)))
   
                                        # los puntos rojos están cada vez más cerca
   
   
   
   # ahora usamos los datos propios y exploramos el ajuste del modelo
   
   
   
   sim1 %>%                     # estos son los datos de la base sim1
   ggplot(aes(x,y)) +           # comenzar con los datos de la base sim1
   geom_point(size = 2, color = 'grey40') + # fijamos el color y el tamaño de los datos de sim1   
   geom_abline(                 # comezamos a trazar las líneas rectas  
     aes(intercept = a0, slope = a1, color = -rmse), 
     data = filter(grid, rank(rmse) <= 10)
   )
   
   
   
   #-------------- Método de Newton rapson
   
   #--------optim(valor incial, función a min, la fuente de data)
    best <- optim(c(0,0), rmse, data = sim1)
   
    
    # De los valores que me interesan, nos fijamos en la la sublista par
    best[[1]]
   
    
    sim1 %>% 
      ggplot(aes(x,y)) +
      geom_point(size = 2, color = 'grey30') + 
      geom_abline(
        intercept = best[[1]][1], 
        slope = best[[1]][2],
        color = 'red'
      )
    
    
    # este enfoque nos permite hacer generalizaciones 
    # y = a0 + a1*x1 + a2*x2 + a3*x3 + ... + an*xn
    
    lm(y ~ x, data = sim1) -> sim1_mod
    
    coef(sim1_mod)
    
    summary(sim1_mod)
    
    

# Sesión 161: Visualización de predicciones y residuos --------------------

    # residuos nos permiten localizar patrones
     # qué pasa si los errores no se distribuyen normal? qué nos dice esto?
    
    
  grid <- sim1 %>% 
      data_grid(x)   # la columna de x y genera una parrilla de valores de la variable
    
   grid <- grid %>% 
     add_predictions(sim1_mod) # agrega una columna con las predicciones
   
   
   
   sim1 %>% 
     ggplot(aes(x)) +
     geom_point(aes(y = y)) +
     geom_line(aes(y = pred), data = grid, color = 'red', size = 1)
   
   
   sim1 <- sim1 %>% 
     add_residuals(sim1_mod)    # agregar residuos sobre el data set original
   
   
   sim1 %>% 
     ggplot(aes(resid)) + 
     geom_freqpoly(binwidth = 0.5) # queremos que siga una normal
                                   # si no pasa esto quiere decir que hay información que estamos deseetimando
                                   # hay info que no ha sido explicado
                                   # queremos que se acercque lo más posiboe a la normal 0
   
   
   sim1 %>% 
     ggplot(aes(x, resid)) + 
     geom_ref_line(h = 0) +
     geom_point()                  # como no hay relación entre x y los residuos podemos decir que es un buen modelo
                                    # para explicar la tendencia que tienen los datos
                                    # el residuo es aleatorio visualmente
     

# Sesión 163 : Fórmulas y modelos -----------------------------------------

   
   # y ~ x <-> y = a0 + a1 * x   <-> a0 *out0 + a1*out1
   
   
   df <- tribble(
     ~y, ~x1, ~x2,
      4,   2,   5,
      5,   1,   6
   )
   
   model_matrix(df, y ~ x1)
   model_matrix(df, y ~ x1 -1)

   
   lm(y ~ x1, df )
   
   ?model_matrix
   
   
   model_matrix(mtcars, mpg ~ cyl) %>%  View()
   
   lm(mpg~cyl, mtcars)
   
   
     mtcars %>% select(mpg, cyl) %>%  View()
   
   

# Sesión 163: Modelos según tipo de variable ------------------------------

     
    # Variables categóricas
    # y ~ sex <-> y = a0 + a1*sex_male (sexmale = valores 0, 1)
     
     
     df <- tribble(
         ~sex,    ~value,
       'male',      1,
     'female',      5,
       'male',      1
      )
     
     model_matrix(df, value ~ sex )
  
   # segundo data set
     
     sim2 %>% 
       ggplot(aes(x,y)) + 
       geom_point()
     
   
   mod2 <- lm(y ~ x, sim2)
   
   grid <- sim2 %>% 
          data_grid(x) %>% 
          add_predictions(mod2)
   
   grid
    
    
   sim2 %>% 
     ggplot(aes(x)) +
     geom_point(aes(y=y)) +
     geom_point(data = grid, aes(y = pred), color = 'red', size = 4)
    
    sim3 %>% 
      ggplot(aes(x1,y)) +
      geom_point(aes(color = x2))
    
   mod1 <- lm(y ~ x1 + x2, data = sim3) # efecto independiente de cada variable
   
   mod2 <- lm(y ~ x1 * x2, data = sim3) # si quiero ver interacciones
   
   # y = a0 + (a1 * x1 + a2 * x2) + [a12 * x1 * x2]
   
   
   
   
   grid <- sim3 %>% 
     data_grid(x1, x2) %>% 
     gather_predictions(mod1, mod2)    # te añade las predicciones en filas
   
  grid   
   
   
  grid <- sim3 %>% 
    data_grid(x1, x2) %>% 
    spread_predictions(mod1, mod2)    # te añade las predicciones en columnas

  
    grid %>% View()   
  
    sim3 %>% 
      ggplot(aes(x1,y, color = x2)) +
      geom_point() + 
      geom_line(data = grid, aes(y = pred)) + 
      facet_wrap(~model)
    
    sim3 %>% 
      gather_residuals(mod1,mod2) %>% 
      ggplot(aes(x1, resid, color = x2)) + 
      geom_point() +
      facet_wrap(model ~ x2, nrow = 4, ncol = 4)
    
    ?facet_wrap
    
    
    
    # modelos de variables continuas con interacción
    
        mod1 <- lm(y ~ x1 + x2, data = sim4)
    mod2 <- lm(y ~ x1 * x2, data = sim4 )
  
    
    
    grid <- sim4 %>% 
      data_grid(
        x1 = seq_range(x1, 5),    # creando una secuencia de números entre valores del dom de la var
        x2 = seq_range(x2, 5)
      )
    
    grid
    
    
    
    seq_range(c(0.23675, 0.98765), n = 6)
    seq_range(c(0.23675, 0.98765), n = 6, pretty = TRUE) #
    
    x1 <- rcauchy(1000)    # colas largas en esta distribución
  
      hist(x1)
    seq_range(x1, n = 10, trim = 0.25)
    
    x2 <-c(0,1)
    
    seq_range(x2, n = 10)
    seq_range(x2, n = 10, expand = 0.1)   # expand expande el universo de datos en porcentaje
    seq_range(x2, n = 10, expand = 0.25)  # note que lo hace a través de colas
    seq_range(x2, n = 10, expand = 0.5)
    
    
    grid <- sim4 %>% 
      data_grid(
        x1 = seq_range(x1, 5),    # creando una secuencia de números entre valores del dom de la var
        x2 = seq_range(x2, 5)
      ) %>% 
      gather_predictions(mod1, mod2)  # vamos a añadir las predicciones para las dos columnas,
                                      # recuerda que con gather se añade en las últimas filas
    
    grid
    
    
    
    
    grid %>% 
      ggplot(aes(x1, x2)) +
      geom_tile(aes(fill = pred)) +
      facet_wrap(~model)
      
    grid %>% 
      ggplot(aes(x1, pred, color = x2, group = x2)) +
      geom_line() +
      facet_wrap(~model)
    
    grid %>% 
      ggplot(aes(x2, pred, color = x1, group = x1)) +
      geom_line() +
      facet_wrap(~model)
    
    
    
    sim4 %>% 
      gather_residuals(mod1,mod2) %>% 
      ggplot(aes(x2, resid, color = x2)) + 
      geom_point() +
      facet_wrap(~model)
    
    

# Sesión 164: Transformaciones de variables -------------------------------------------

    # Qué ocurre si tenemos una especificación diferente que integre log, valores al cudrado, raíces, etc.
    # log(y) ~ sqrt(x1) + x2 <-> log(y) = a0 + a1*sqrt(x1) + a3 * x^2
    # y ~ x + I(x ^ 2) <-> y = a0 + a1 * x + a2 * x^2   # cuidado con la redundancia
    
    
    df <- tribble(
      ~y,    ~x,
       1,     1,
       2,     2,
       3,     3
    )
    
    model_matrix(df, y ~ x + x^2)
    model_matrix(df, y ~ x + I(x^2))
    ?model_matrix  
    
    # y = a0 + a1*x + a2* x^3 + ... 
    
    model_matrix(df, y ~ poly(x,2))
    
    library(splines)
    model_matrix(df, y ~ ns(x,2))
  
    set.seed(1234)
    sim5 <- tibble(
      x = seq(0, 3.5 *pi, length = 50), 
      y = 4*sin(x) + rnorm(length(x))
    )
    
    
    
    mod1 <- lm(y ~ ns(x,1), sim5)
    mod2 <- lm(y ~ ns(x,2), sim5)
    mod3 <- lm(y ~ ns(x,3), sim5)
    mod4 <- lm(y ~ ns(x,4), sim5)
    mod5 <- lm(y ~ ns(x,5), sim5)
    mod6 <- lm(y ~ ns(x,6), sim5)
    
    
    grid <- sim5 %>% 
     data_grid(x = seq_range(x, n = 50, expand = 0.1)) %>% 
     gather_predictions(mod1, mod2, mod3, mod4, mod5, mod6, .pred = 'y')
    
    
    sim5 %>% 
      ggplot(aes(x,y)) + 
      geom_point(color = 'grey50') +
      geom_line(aes(x,y), data = grid, color = 'purple') +
      facet_wrap(~model)
    
    #  Veamos qué ocurre con polinomios
    
    mod1 <- lm(y ~ poly(x,1), sim5)
    mod2 <- lm(y ~ poly(x,2), sim5)
    mod3 <- lm(y ~ poly(x,3), sim5)
    mod4 <- lm(y ~ poly(x,4), sim5)
    mod5 <- lm(y ~ poly(x,5), sim5)
    mod6 <- lm(y ~ poly(x,6), sim5)


    grid <- sim5 %>% 
      data_grid(x = seq_range(x, n = 50, expand = 0.5)) %>% 
      gather_predictions(mod1, mod2, mod3, mod4, mod5, mod6, .pred = 'y')
    
    
    sim5 %>% 
      ggplot(aes(x,y)) + 
      geom_point(color = 'grey50') +
      geom_line(aes(x,y), data = grid, color = 'purple') +
      facet_wrap(~model)
    

# Sesión 165: NAs en el modelo --------------------------------------------
#-----
x <- c(2,3,1,2, 0.4,4)
which.min(x)  
which.max(x)
    
#----


 # EN CASO DE TENER VALORES FALTANTES CARGAR ESTA INSTRUCCIÓN
options(na.action = na.warn)

 # CREANDO UNA INSTRUCCIÓN CON VALORES NAS

df <- tribble(
  ~x,    ~y,
   1,    1.5,
   2,    NA,
   3,    3.5,
   4,    7.5,
  NA,    15
)


 # VEAMOS QUÉ OCURRE SI APLICAMOS EL MODELO


mod <- lm(y ~ x, data = df)


# Warning message:
# Dropping 2 rows with missing values

 # Si quiero excluir los valores del análisis sin una advertencia
mod <- lm(y ~ x, data = df, na.action = na.exclude)

# número de observaciones
nobs(mod)


# Sesión 166: Otros modelos no lineales -----------------------------------

#   y = a0 + a1*x1 + a2*x2 + ... + an*xn
#Modelo lineal generalizado
stats::glm()

#Modelo generalizado aditivo
mgcv::gam() # y~s(x) <-> y = f(x)

#Modelo lineal penalizado
glmnet::glmnet

# Modelo lineal Robusto
MASS::rlm

# Árboles y bosques aleatorios

rpart::rpart()
randomForest::randomForest()
xgboost::xgboost()



# caso de estudio
# Sesión 168: Los diamantes más malos son los más caros --------------------------------------------------------------


# Por qué los diamantes de baja calidad son los más caros


diamonds %>% ggplot(aes(cut, price)) + geom_boxplot()
diamonds %>% ggplot(aes(color, price)) + geom_boxplot()
diamonds %>% ggplot(aes(clarity, price)) + geom_boxplot()


diamonds %>% ggplot(aes(carat, price)) + geom_hex(bins= 50)



diamonds2 <- diamonds %>% 
  filter(carat <= 2.5) %>% 
  mutate(lprice = log2(price), lcarat = log2(carat))

diamonds2 %>% 
  ggplot(aes(lcarat, lprice)) + 
  geom_hex(bins = 50)


mode_diamonds <- lm(lprice ~ lcarat, data = diamonds2)

grid <- diamonds2 %>%
  data_grid(carat = seq_range(carat, 30)) %>% 
  mutate(lcarat = log2(carat)) %>% 
  add_predictions(mode_diamonds, 'lprice') %>% 
  mutate(price = 2^ lprice)

grid  


diamonds2 %>%
  ggplot(aes(carat, price)) + 
  geom_hex(bins = 50) +
  geom_line(data = grid, color = 'red', size = 1)           
  

diamonds2 <- diamonds2  %>% 
  add_residuals(mode_diamonds, 'lresid')


diamonds2 %>% 
  ggplot(aes(lcarat, lresid)) + 
  geom_hex(bins = 50)



diamonds2 %>% ggplot(aes(cut, lresid)) + geom_boxplot()
diamonds2 %>% ggplot(aes(color, lresid)) + geom_boxplot()
diamonds2 %>% ggplot(aes(clarity, lresid)) + geom_boxplot()



# Sesión 169: Mejorando el modelo de diamantes ----------------------------


mod_diamonds <- lm(lprice ~ lcarat + color + cut + clarity, data = diamonds2)

plot1 <- diamonds2 %>% 
        data_grid(cut, .model = mod_diamonds) %>% 
  add_predictions(mod_diamonds) %>% ggplot(aes(cut, pred)) + 
  geom_point()

diamonds2 <- diamonds2 %>% 
  add_residuals(mod_diamonds, 'lresid2')

ggplot(diamonds2, aes(lcarat, lresid2)) + 
  geom_hex(bins = 50)



  diamonds2 %>% 
  filter(abs(lresid2)>1) %>% 
  add_predictions(mod_diamonds) %>% 
  select(price, pred, carat:table, x:z) %>% 
  arrange(price)
  




plot2 <- diamonds2 %>% 
  data_grid(clarity, .model = mod_diamonds) %>% 
  add_predictions(mod_diamonds) %>% ggplot(aes(clarity, pred)) + 
  geom_point()


plot3 <- diamonds2 %>% 
  data_grid(color, .model = mod_diamonds) %>% 
  add_predictions(mod_diamonds) %>% ggplot(aes(color, pred)) + 
  geom_point()


# grafico donde se vea el precio del diamante en función de carat, cut y color

library(ggpubr)
library(gridExtra)
grid.arrange(plot1,plot2, plot3)
ggarrange(plot1,plot2,plot3, nrow = 3, labels = c('A: Cut', 'B: Clarity', 'C: Color'))


diamonds2 %>% 
  data_grid(clarity, .model = mod_diamonds) %>% 
  add_predictions(mod_diamonds) %>% ggplot(aes(clarity, pred))+
  facet_wrap(~cut)

# Sesión 170: el problema de los vuelos diarios ---------------------------

# qué afecta el número de vuelos de un día?

daily <- flights %>% 
               mutate(date = make_date(year, month, day)) %>% 
               group_by(date) %>%
               summarise(n = n())
  
daily %>% 
  ggplot(aes(date, n)) + geom_line()


daily %>% 
  mutate(wday = wday(date, label = TRUE, abbr = TRUE)) -> daily


# boxplot entre variabñles categóricas y continuas
 # una disminución el grande los sábados y se recupera un poco del domingo

daily %>% 
        ggplot(aes(wday, n)) +
        geom_boxplot()


# un modelo 
      
      mod <- lm(n ~ wday, data = daily) 
      
grid <- daily %>%
        data_grid(wday) %>%
        add_predictions(mod, 'n')
        
  # graficamos

 daily %>% 
   ggplot(aes(wday, n)) + 
   geom_boxplot() + 
   geom_point(data = grid, color = 'red', size = 4)

# veamos qué ocurre con los errores
 daily <- daily %>% 
   add_residuals(mod)

 daily %>%                       # hay algunos patrones que el modelo no ha sido capaz de explicar
   ggplot(aes(date, resid, color = wday)) + 
   geom_ref_line(h = 0) + 
   geom_line() + 
   facet_wrap(~wday)
 
 
 # fechas que el modelo no predice correctamente
 
 daily %>% 
   filter(abs(resid)> 100)
 
 # el modelo no es capaz de explicar fechas importantes
    daily %>% 
      ggplot(aes(date, resid)) + 
      geom_ref_line(h = 0) + 
      geom_line(color = 'grey30') + 
      geom_smooth(se  = T, span = 0.2)

 

# Sesión 171: vuelos por día de la semana y trimestre ---------------------

    
    # trabajando con los sábados
    
    
    
    daily %>%
      filter(wday == 'sá\\.') %>% 
      ggplot(aes(date, n)) + 
      geom_point() + 
      geom_line() +
      scale_x_date(NULL, date_breaks = '1 month', date_labels = '%b')
      
    
    # 
    # 5 junio - 25 agosto
    
    
    term <- function(date) {
      cut(date, 
          breaks = ymd(20130101, 20130605,20130825,20140101),
          labels = c('spring', 'summer', 'fall'))
    }
    
    
    daily <- daily %>% 
      mutate(term = term(date))
 
    
    daily %>% 
      filter(wday == 'sá\\.') %>% 
      ggplot(aes(date, n, color = term)) + 
      geom_point(alpha = 0.25) + 
      geom_line() + 
      scale_x_date(NULL, date_breaks = '1 month', date_labels = '%b')
    
    
    # veamos como estás nuevas variables afectan a los días de la semana
    
    daily %>% 
      ggplot(aes(wday, n, color = term)) + 
      geom_boxplot()
    
    # el boxplot muestra que sí hay una dependencia dependiendo del trimestre por cada día
    
    # Evaluando modelos
    
    mod1 <- lm(n ~ wday, data = daily)
    mod2 <- lm(n ~ wday*term, data = daily)
      
    daily %>% 
      gather_residuals(without_term = mod1, with_term = mod2) %>% 
      ggplot(aes(date, resid, color = model)) +
      geom_line(alpha = 0.7)
      
    
    # viendo las predicciones
    
    
    grid <- daily %>% 
           data_grid(wday, term) %>% 
           add_predictions(mod2, 'n')
    
    daily %>% 
      ggplot(aes(wday, n)) + 
      geom_boxplot() + 
      geom_point(data = grid, color = 'red') + 
      facet_wrap(~term)

    
    
    # MODELOS LINEALES ROBUSTOS EN PRESENCIA DE OUTLAYERS
    
    summary(mod1)
    mod3 <- MASS::rlm(n ~ wday*term, data = daily)
    
    daily %>% 
      add_residuals(mod3, 'resid') %>% 
      ggplot(aes(date, resid)) +
      geom_hline(yintercept = 0, size = 2, color = 'white') + 
      geom_line()
      
    

# Sesión 172: Modelos múltiples -------------------------------------------

    compute_vars <- function(data) {
      data %>% 
        mutate(
          term = term(date),
          wday = wday(date, label = T)
        )
    }
    
    wday2 <- function(x) wday(x, label = TRUE)
    mod4 <- lm(n ~ wday(date) * term(date), date = daily)
    
    
    library(splines)
    
    mod <- MASS::rlm(n ~ wday * ns(date,5), data = daily)
    
    daily %>% 
      data_grid(wday, date = seq_range(date, n = 13)) %>% 
      add_predictions(mod) %>% 
      ggplot(aes(date, pred, color = wday)) + 
      geom_line() + geom_point()
    

# Sesión 173: Gapminder del Doctor Hans Rosling ---------------------------

    # Instalé gapminder
    
  library(gapminder)  
    
    # data set de gapminder
gapminder::gapminder
?gapminder        


gapminder %>% 
  ggplot(aes(year, lifeExp, group = country)) + 
  geom_line(alpha = 0.2)



unique(gapminder$country)

Chile <- filter(gapminder, country == 'Chile')

Chile %>% 
  ggplot(aes(year, lifeExp)) + 
  geom_line() +
  ggtitle('Full data') + 
  theme(plot.title = element_text(hjust = 0.5))



ch_mod <- lm(lifeExp ~ year, data = Chile)

Chile %>% 
  add_predictions(ch_mod) %>% 
  ggplot(aes(year, pred)) + 
  geom_line() +
  ggtitle('Full Data - Chile') + 
  theme(plot.title = element_text(hjust = 0.5))


Chile %>% 
  add_residuals((ch_mod)) %>% 
  ggplot(aes(year, resid)) +
  geom_hline(yintercept = 0, color = 'white', size = 3) + 
  geom_line()+ 
  ggtitle('Residual Pattern') + 
  theme(plot.title = element_text(hjust = 0.5))


# Sesión 174: El data frame anidado ---------------------------------------

    by_country <- gapminder %>% 
               group_by(country, continent) %>% 
               nest()
    
   # by_country %>% str()
    
    
    by_country$data[1]
    
    
    

# 175: Modelando y operando con data frames anidados ----------------------

    
    country_model <- function(df){
      lm(lifeExp ~ year, data = df)
    }
    
    
    models <- map(by_country$data, country_model)
    
    
    by_country <- by_country %>% 
      mutate(model = map(data, country_model))
    
    
    by_country %>%  View()
    

    by_country %>% 
      arrange(continent, country)
    
    
    
   by_country <-  by_country %>% 
      mutate(resids = map2(data, model, add_residuals))
    
   
   
    # vamos a comvertir de vuelta
   
   resids <- unnest(by_country, resids)
   
   resids %>% 
     ggplot(aes(year, resid)) + 
     geom_line(aes(group = country), alpha = 0.25) + 
     geom_smooth(se =F) + 
     facet_wrap(~continent, ncol = 2)
    
    ?facet_wrap
    

# Sesión 176: Midiendo la calidad del modelo con broom --------------------

   library(broom)
   glance(ch_mod)
   
   
  glance <-  by_country %>% 
     mutate(glance = map(model, glance)) %>% 
     unnest(glance, .drop = TRUE)
   
  glance %>% 
    arrange(r.squared)
  
   glance %>% 
     ggplot(aes(continent, r.squared)) + 
     geom_jitter(width = 0.5)
   
   
   
   bad_fit <- filter(glance, r.squared < 0.25)
   
   gapminder %>% 
     semi_join(bad_fit, by = 'country') %>% 
     ggplot(aes(year, lifeExp, color = country)) + 
     geom_line()
   
   

# Sesió 177: Columnas de listas -------------------------------------------

   
   data.frame(x = list(1:3, 4:6))
   
   
   data.frame(x = I(list(1:3, 4:6)),
              y = c('1,2,3', '3,4,5'))
   # * no es la mejor forma de trabajar con listas
   # *  mejor trabajar con tibble
   
   tibble(x = list(1:3, 4:6),
              y = c('1,2,3', '3,4,5'))
   
   
   # Crear columna de listas con nest(), summarise() + lis(),
   # mutate() + map o map2 o pmap()
   # unnest() 
   
   # tibble::enframe()
   
   

# Sesión 178: Generando columnas de listas --------------------------------

    # diferentes formas de crear columnas de listas
   
   # nest() crea un df anidado
   
gapminder %>% 
     group_by(country, continent) %>% 
     nest()
  
   # o
   
   gapminder %>% 
     nest(year:gdpPercap)
   
 df <- tribble(
   ~x, 
     'a, b, c',
     'f, g, h',
   ' w, x, y, z'
   
 )  
 
   df %>% 
     mutate(y = str_split(x, ','))  %>% 
     unnest()
   
   
   
   # tidy::separate_rows
   
   
   sim <- tribble(
     ~f,  ~params,
     'runif', list(min = -1, max = 1),
     'rnorm', list(sd = 3),
     'rpois', list(lambda = 5)
   ) 
   
   sim %>% 
     mutate(sims = invoke_map(f, params, n = 10))
   
   probs <- c(0.01, 0.25, 0.5, 0.75, 0.99)
   
   mtcars %>% 
     group_by(cyl) %>% 
     summarise(p = list(probs),
               q = list(quantile(mpg, probs))) %>% 
                                                    unnest()
   x <- list(
     a = 1:6,
     b = 3:4,
     c = 5:8
   )
   
     
   df <- enframe(x)
   df
   
   
   df %>% 
     mutate(
       smry = map2_chr(name, value,
                       ~ stringr::str_c(.x, ': ', .y[1]))
     )
   
   
   quantile(mtcars$mpg)
   
   
   

# Sesión 179: De columnas de listas al dato original ----------------------

   
   df <- tribble(
     ~x, 
     letters[1:8],
     2:9,
     runif(6)
   )
   
   df %>% 
     mutate(
       type = map_chr(x, typeof),
       length = map_int(x, length)
     )
   
  df <- tribble(
    ~x, 
    list(a = 1, b = 2),
    list(a = 2, c = 4),
    list(a = 3, c = 8)
  )
  
  df %>% 
    mutate(
      a = map_dbl(x, 'a'),
      b = map_dbl(x, 'b', .null = NA_real_),
      c = map_dbl(x, 'c', .null = NA_real_)
    )
    
  
  
  tibble(x = 1:2,
         y = list(1:8, 1)) %>% unnest(y)
  
  
  df1 <- tribble(
    ~x,       ~y,        ~z,
     1,     c('x', 'y'),   1:2,
     2,        'm',         3
  )
  
  
  
  df1 %>%  unnest(y)
  df1 %>%  unnest(z)
  
  
  df1 %>%  unnest(c(y,z))  # se puede hacer unnest siempre que tengan la misma longitud
  
  # ejemplo
  
  df2 <- tribble(
    ~x,       ~y,        ~z,
    1,     c('x', 'y', 'z'),   1:2,
    2,        'm',         3:5
  )
  
  
  df2 %>% unnest(y)
  df2 %>% unnest(z)
  
  df2 %>% unnest(c(y, z))  # las longitudes son incompatibles
  
  
  # BROOM
  # glance(model)
  glance(ch_mod)
  
  # tidy(model)
  
  tidy(ch_mod)
  # augment(model, data)
  
  
  augment(ch_mod, Chile)
  
  getwd()

  
  
  
  
    