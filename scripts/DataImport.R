#Importación de datos
#Paquete readr 

library(tidyverse)

# Ficheros en data frames
# Se le llama fichero a un conjunto de información clasificada y
# almacenada de diversas formas para su conservación y fácil acceso en cualquier momento. 
# En informática, un archivo o fichero también es un conjunto de información que
# se almacena en forma virtual para ser leído y/o accedido por medio de una computadora.

# * read_csv() `,` 
# * read.csv() `;`
# * read_tsv() `\t`
# * read_delim(delim = `\n`)

# * read_fwf() # anchura fija
  # fwf_widths()
  # fwf_positions()
# * read_table() 

# * read_log() 
# install.packages(`webreadr`)

# en este curso se enfoca en la primera csv

write.csv(mtcars, file = "../Desktop/Directorio/CIDE/Trabajos de R/Tidyversecurso/data/mtcars.csv")


#leyendo el archivo
getwd()

cars <- read_csv("../Desktop/Directorio/CIDE/Trabajos de R/Tidyversecurso/data/mtcars.csv")

View(cars)

cars


#generar una tibble con read


# primera columna nombre de varibale, pero el problema es cuando tenemos un metadato. cómo se construyó
read_csv("x, y, z  
          1,2,3
          4,5,6
          7,8,9")

# añademos el parámetro skip

read_csv("me valen
            todos
            los datos
          x, y, z  
          1,2,3
          4,5,6
          7,8,9", skip = 3)

read_csv("#esto es un comentario
          x, y, z  
          1,2,3
          4,5,6
          7,8,9", comment ="#")

# otra forma de crear estos datos es

read_csv("1,2,3\n4,5,6\n7,8,9", col_names = FALSE)


read_csv("1,2,3\n4,5,6\n7,8,9", col_names = c("PRIMERA", "SEGUNDA", "TERCERA"))


read_csv("x,y,z\n1, 2,.\n.,.,.,6", col_names = c("PRIMERA", "SEGUNDA", "TERCERA"),na = ".")


read_csv("x,y,z\n1, 2,.\n4,.,#,6", col_names = c("PRIMERA", "SEGUNDA", "TERCERA"),na = c(".","#"))



# porque ocupar read_csv y no read.csv ... las funciones de readr son más rápidas que del paquetebase
# readr produce tibbles
# la transformacion de los datos es estocastica en el paquete base
# las tibbles viniero a solucionar
# las funciones del paquete base no funcionan igual
# readr es reproducible






#ASSIGMENT 15

#Questions for this assignment
#Indica qué función y parámetros usarías para leer ficheros separados con "|"

?read_delim
 # Podemos usar la función read_delim() y utilizando el argumnto sep"|" o con el parámetro delim = "|"
read_delim(I("a|b\n1.0|2.0\n3.0|4\n1.0|M amigo tonto"), delim = "|")


#Además de file, skip y comment que hemos visto en el curso, ¿qué otros argumentos tiene read_csv y read_tsv en común?
#Indica para que sirve cada uno de ellos.

#otros argumentos, son: 

    # delim : Single character used to separate fields within a record.
    # quote : Corresponde a un caracter que necesita ser encomillado
    # escape_backslash: 
    # escape_double
    # col_names: indica el nombre de las columnas
    # col_types : indica el tipo o clase de columna c = character i = integer, n = number, d = double, l = logical, f = factor
    #D = date, T = date time, t = time. 

read_csv("x,y\n1,2\n3,4", col_types = "ii")

    # locale : permite controlar la configuración oara una eegión, como zona horaria, codificación, la marca decimal, día mes
    # quoted_na: los valores perdidos entre comillas deben tratarse como valores perdidos
    # trim_ws: variable logica que inidca que deben cortarse los espacios en blanco de cada campo
    # n_max:  max numero registrado que puede leerse
    # guess_max: maximo numero de registros usados para adivinar el numero de columnas
    # progress: mostrar una barra de progreso si el tiempo de lectura es 5 segundos o mas
    # skip_empty_rows: si hay filas completas como NA no apareceran  mientras les pongamos TRUE


#Indica los argumentos más importantes de read_fwf()

?read_fwf
# los argumnetos más importantes de read_fwf() son file, col_positions, col_types, 


#A veces un csv contiene necesariamente comas en los campos que son strings. Para evitar problemas en la carga,
#suelen ir rodeadas de comillas dobles " o de comillas simples '. La convención de read_csv() es que asume que 
#cualquier caracter vendrá rodeado por comillas dobles " y si lo queremos cambiar tenemos que usar la función read_delim(). 

#Indica qué argumentos tendríamos que especificar para poder leer el texto del siguiente data frame correctamente

"x,y\n1,'a,b'"

# podemos usar el argumento quote = "\"'""

#Indica qué está mal en la siguiente línea de lectura de CSV:

  

  # La dimesión es errónea o aumentamos una columna o eliminamos un fila
  
#Indica qué está mal en la siguiente línea de lectura de CSV:
  
  read_csv("x,y\n1,2,3\n4,5,6")

  read_csv("x,y\n1,2\n3,4\n5,6")
  
  # falta un elemento para complemtar correctamente la tibble 2\n3 y 4\n5

#Indica qué está mal en la siguiente línea de lectura de CSV:
  
  read_csv("x,y\n\"1")

  read_csv("x, y\n, 1")
  
  # era necesario agregar una coma y eliminar una comilla para generar la tibble
  
  
  #Indica qué está mal en la siguiente línea de lectura de CSV:
  


  read_csv("x,y\n1,2\na,b")

  read_csv("x,y\n1,a\n2,b")
  
  # en principio no hay warnings pero podemos cambiar el orden de las observaciones para tener dbl o chr
  
  
  
  #Indica qué está mal en la siguiente línea de lectura de CSV:
  
  read_csv("x;y\n1;2")

  # acá podemos resolverlo con una coma 

  read_csv("x,y\n1,2")
  read_csv2("x;y\n1;2")
  

  ?read_csv
  
  
  # Indica qué función y parámetros usarías para leer ficheros separados con "|"
  
  ##read_delim(file, delim = "|")
  #Además de file, skip y comment que hemos visto en el curso, ¿qué otros argumentos tiene read_csv y read_tsv en común? Indica para que sirve cada uno de ellos.
  
  #De la documentación oficial:
    
  #  col_names and col_types are used to specify the column names and how to parse the columns
  #locale is important for determining things like the encoding and whether "." or "," is used as a decimal mark.
  #na and quoted_na control which strings are treated as missing values when parsing vectors
  #trim_ws trims whitespace before and after cells before parsing
  #n_max sets how many rows to read
  #guess_max sets how many rows to use when guessing the column type
  #progress determines whether a progress bar is shown.
  #Indica los argumentos más importantes de read_fwf()
  
  #El más importante de los argumentos de read_fwf para ficheros "fixed-width formats", es col_positions que nos indica en qué posiciones empieza cada una de las columnas de datos.
  
  #A veces un csv contiene necesariamente comas en los campos que son strings. Para evitar problemas en la carga, suelen ir rodeadas de comillas dobles " o de comillas simples '. La convención de read_csv() es que asume que cualquier caracter vendrá rodeado por comillas dobles " y si lo queremos cambiar tenemos que usar la función read_delim(). 
  
  #Indica qué argumentos tendríamos que especificar para poder leer el texto del siguiente data frame correctamente
  
  "x,y\n1,'a,b'"
  
  data <- "x,y\n1,'a,b'"
  read_delim(data, ",", quote = "'")
  #Indica qué está mal en la siguiente línea de lectura de CSV:
    
    read_csv("x,y\n1,2,3\n4,5,6")
  
  #Solo hay dos columnas en la cabecera pero tres en los datos. Se eliminará la última columna de datos para cada fila de información.
  
  #Indica qué está mal en la siguiente línea de lectura de CSV:
    
    read_csv("x,y,z\n1,2\n3,4,5,6")
  
  #Las filas de datos tienen menos o más columnas de información de la cabecera. En la lectura, la primera fila tendrá un NA mientras que la última eliminará la última columna de información.
  
  #Indica qué está mal en la siguiente línea de lectura de CSV:
    
    read_csv("x,y\n\"1")
  
#  El caracter escapante está mal indicado.
  
 # Indica qué está mal en la siguiente línea de lectura de CSV:
    
    read_csv("x,y\n1,2\na,b")
  
  
  #Las columnas de datos no son homogeneas: contienen números y caracteres a la vez.
  
  #Indica qué está mal en la siguiente línea de lectura de CSV:
    
    read_csv("x;y\n1;2")
  
  
  #Si el separador es un punto y coma, debemos usar read_csv2 en lugar del estándar.
  
    
# Sesion 80    
  # Funciones con Parse_ transforman el tipo de texto en el tipo de dato
    
    str(parse_logical(c("TRUE","FALSE", "FALSE", "NA"))) # str es strucrure evalua valores log
    
    str(parse_integer(c("1","2","3","4")))
    
    str(parse_date(c("1678-09-02","1977-09-05")))
  
    #añadiendo un segundo argumento
    
    parse_integer(c("1","2","3","#"), na = "#")

    
   DATA <-  parse_integer(c("1","2","3.146178","HOLA"))

   problems(DATA)    
   
   # estudiando los procesadores
   #parse_logical()
   #parse_integer()
   #parse_double()
   #parse_number()  este es más flexible, dif separadores y notación
   
   #parse_character() 
   
   #parse_factor()
   #parse_datetime()
   #parse_date()
   #parse_time()
   
   #Sesion 81: procesado de texto
   
   #familia de los número, procesado de números
   # la gentes escibe comas o puntos (decimales)
   # caracteres que rodean el numero (los valores de la moneda)
   # agrupaciones de los numeros (1,000,000)
   
   parse_double(("12.345"))
   parse_double("12,345", locale = locale(decimal_mark = ","))   
   
   #problemas con porcentajes
   
   parse_number("100€")
   parse_number("$1000")

   #agrupaciones
   
   parse_number("12%")

   #una frase con un número 
   
   parse_number("el vestido ha costado 3000€")
   
   # problemas con el seprador
   
   parse_number("$1.000.000", locale = locale(grouping_mark = "."))
   
    # en suiza....?
   parse_number("$1'000'000", locale = locale(grouping_mark = "'"))
   
   #Sesion 82
   # procesado de string
    # parse_character()
   
   charToRaw("alonso pizarro") # cofificacion a hexadecimal ASCII
   #B1 <- 
   
   # latin (ISO-8859-1) PARA IDIOMAS DE EURPA DEL ESTE
   # latin (ISO-8859-2) PARA IDIOMAS DE EURPA DEL OESTE 
   
  
   #SESION 83
   
   
   MONTHS <- c("JAN", "FAB", "MAR", "ABR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOB", "DEC")
   
   
   parse_factor(c("MAY", "AUG", "SEP"), levels = MONTHS)
   
   
   # procesadores de fechas
   
   #EPOCH <- 1970-01-01 00:00
   
   #parse_datetime(ISO-8601)
   
   parse_datetime("2018-06-05T1845")
    parse_datetime("02180605")   
   
    parse_date("2015-12-07")
    parse_date("2015/12/07")

    parse_time("03:00 pm")    
    parse_time("03:00:08")
    
    
    # Años
    # %Y <- año con 4 digitos
    # %y <-  año con 2 digtos (00-69) <-  2000-2069 ; 70-99 <-  1970-1999
    
    # Meses
    # %m <- mes en formato de dos dígitos 01-12
    # %b <- abreviación del me 'Ene', 'Feb',...
    # %B <- no,be compleyto del mes 'Enero', 'Febrero', ...
    
    #Día
    # %d <-  número del día con dos dígitos 01-31
    # %e <- de fofrma opcional, los digitos de 1-9 pueden llevar espacios en blanco
    
    #Horas
    # %H <- hora entre 0-23
    # %I <- hora entre 0-12 siempre va con %p
    # %p <- am/pm
    # %M <- minutos 0-59
    # %s <- segundos enteros 0-59
    # %0S <- segundos reales
    # %Z <- Zona horaria America/Chicago, canada, france, spain
    #%z <-  zona horaria respecto de la UTC +0800, +0100
    
    # no digitos
    # %. <- eliminar un caracter no digito
    # %* <- eliminar cualquier numero de caracteres que non sean digitos
    
    parse_date("05/08/15", format = "%d/%m/%y")
    parse_date("08/05/15", format = "%m/%d/%y")
    parse_date("01-05-2018", format = "%m-%d-%Y")
    parse_date("01 jan 2018", format = "%d %b %Y")
    parse_date("03 March 17", format = "%d %B %y")
    
    
    parse_date("05 janvier 2012", format = "%d %B %Y", locale = locale("fr"))
    
    parse_date("3 Septiembre 2014", format = "%d %B %Y", locale = locale("es"))
    
    
    
    #ASSIGMENT 16
    
    
    #Questions for this assignment
    # 1. Investiga la documentación para decir cuales son los argumentos más importantes que trae la función locale()
    
    
    ?locale()
      # Los argumnetos de locale() consisten en date_names, date_format, time_format, decimal mark y grouping_mark
    
    
    # 2. Investiga qué ocurre si intentamos configurar a la vez el decimal_mark y grouping_mark con el mismo carácter. 
    
    parse_number("$1,000,099,001", locale = locale(grouping_mark = ","))
    parse_number("$1,000,099,001", locale = locale(decimal_mark = "."))
    
    # dice que ambos valores deberían ser diferentes. 
    
    ?locale
    
    # ¿Qué valor por defecto toma el grouping_mark cuando configuramos el decimal_mark al carácter de coma “,”? 
      # el valor por defecto que toma es el ".".
    
    #  ¿Qué valor por defecto toma el decimal_mark cuando configuramos el grouping_mark al carácter de punto “.”? 
    
      # el valor debería  ser ","
    
    # 3. Investiga qué hace la opción del  locale() cuando se utiliza junto al date_format y al time_format.
    #Crea un ejemplo que muestre cuando puede sernos útil.
    
      #  Cuando tenemos formatos para fechas que se alejan de los estándar es posible utilizar estas dos 
    # funcones.
    # un ejemplo sacado de la red
     
    locale_custom <- locale(date_format = "Dia %d, Mes %M Ano %y",
                            time_format = "Seg %S Min %M Hora %H")
    #locale custom fue asignado a un formato especial de lectura de fecha y tiempo
    
    # ahora construimos un ejemplo
    
    Ejemplo_Fecha <- c("Dia 03, Mes 02 Ano 99", "Dia 04, Mes 05 Ano 01")
    
    #voy a transformar a una fecha con los valores correctos
    
    parse_date(Ejemplo_Fecha) # veamos que no podemos procesar el string a date porque no tiene una forma standard
    
    # asignamos nuestra manera 
    
    parse_date(Ejemplo_Fecha, locale = locale_custom)
    
    # un ejemplo con el horario
    
    Ejemplo_Hora <- c("Seg 03 Min 02 Hora 01", "Seg 04 Min 05 Hora 02")
    
    parse_time(Ejemplo_Hora)
    parse_time(Ejemplo_Hora, locale = locale_custom)
    
    
    # 4. Crea un nuevo objeto locale que encapsule los ajustes más comunes de los parámetros
    # para la carga de los fichero con los que sueles trabajar.
    
    
        # yo soy de Chile, podría construir una forma que procese las fechas con que normlamente se usan en las empresas
            
          Ejemplo_Fecha_CL <-  ("05 mar, 2021")
          
          lectura_fecha <- locale(date_format = "%d %b, %Y")
              
              #lo correcto sería leer esa fecha
          
          parse_date(Ejemplo_Fecha_CL, locale = lectura_fecha)
    
          
          # 5. Investiga las diferencias entre read_csv() y read_csv2()?
          
          # read_csv() and read_tsv() are special cases of the general read_delim().
          #They're useful for reading the most common types of flat file data, 
          #comma separated values and tab separated values, respectively. read_csv2() uses ;
          #for the field separator and , for the decimal point.
          #This is common in some European countries.
        
      
          
          
    # 6. Investiga las codificaciones que son más frecuentes en Europa y las más comunes en Asia. 
          # Usa un poco de google e iniciativa para investigar acerca de este tema.
    
          
          
       
          ##
    # 7. Genera el formato correcto de string que procesa cada una de las siguientes fechas y horas:

          
          
          
    v1 <- "May 19, 2018"
  
    ejemplo_V1 <- locale(date_format = ("%b %d, %Y"))
    parse_date(v1, locale = ejemplo_V1)
    
    
      v2 <- "2018-May-08"
    
    ejemplo_V2 <- locale(date_format = ("%Y-%b-%d"))  
    parse_date(v2, locale = ejemplo_V2)
      
      v3 <- "09-Jul-2013"
      
    ejemplo_V3 <- locale(date_format = ("%d-%b-%Y"))  
    parse_date(v3, locale = ejemplo_V3)
      
      v4 <- c("January 19 (2019)", "May 1 (2015)")
    
    ejemplo_V4 <- locale(date_format = ("%B %d (%Y)"))  
      parse_date(v4, locale = ejemplo_V4)
    
    
    v5 <- "12/31/18" # Dic 31, 2014
    
    ejemplo_V5 <- locale(date_format = ("%m/%d/%y"))  
      parse_date(v5, locale = ejemplo_V5)
    
    
    v6 <- "1305"
    
      ejemplo_V6 <- locale(time_format = ("%H%M"))  
      parse_time(v6, locale = ejemplo_V6)
    
    v7 <- "12:05:11.15 PM"
    
      ejemplo_V7 <- locale(time_format = ("%H:%M:%0S %p"))  
      parse_time(v7, locale = ejemplo_V7)
    
      # SESIÓN 85 La heurística del parseo
      # 100o filas
      #logico -> integer -> double -> number
      #->time ->date -> datetime -> string
      guess_parser("1992-04-25")
      guess_parser("18:19")
      guess_parser(c("3,6,8,25"))
      guess_parser(c("TRUE", "FALSE"))             
      guess_parser(c("3","4","5"))      
      
      # los problemas de los ficheros la podemos  analizar con problems()
      
      challenge <- read_csv(readr_example("challenge.csv"))
      problems(challenge)
      
      
      #   
      
    

        challenge <- read_csv(readr_example("challenge.csv"),
                            col_types = cols(
                              x = col_double(), # parse_double()
                              y = col_date()    # parse_date()
                            ))
      
        View(challenge)
        tail(challenge)
        
        
        #ojo cuando haya que analizar datos, readr lee a su manera. Pueden existir errores.
        
        
        ?stop_for_problems
        
        
        x <- parse_integer(c("1X", "blah", "3"))
        problems(x)
        
        y <- parse_integer(c("1", "2", "3"))
        problems(y)
        
        
        # Sesión 86: función type_convert
        
        
        challenge2 <- read_csv(readr_example("challenge.csv"),
                               guess_max = 1001)    
        
        challenge3 <- read_csv(readr_example("challenge.csv"),
                               col_types = cols(.default = col_character()))    
        
        
        type_convert(challenge3)
        
        df <- tribble(
          ~x , ~y,
          "1", "1.2",
          "2", "2.3",
          "3", "3.125"
        
          )
        
        type_convert(df)
        
        
        #read_lines() lee filas
        read_lines(readr_example("challenge.csv"))
        
        #read_file()
        
        read_file(readr_example("challenge.csv"))
        
        
        
        #Sesion 87: otro tipo de ficheros especiales

        #Escritura de ficheros
        #write_csv(), write_tsv()
        # string en UTF-8
        # date/datetimes ISO8601
        # write_excel_csv()
        
        write_csv(challenge, path = "../Desktop/Directorio/CIDE/Trabajos de R/Tidyversecurso/data/challenge.csv")
        
        # ahora si yo quiero leerlo de nuevo, veamos qué ocurre
        
        read_csv("../Desktop/Directorio/CIDE/Trabajos de R/Tidyversecurso/data/challenge.csv", guess_max = 1001)
        
        # volvemos con el mismo problema de procesamiento. Para esto podemos utilizar un formato alternativo
        
        write_rds(challenge, path = "../Desktop/Directorio/CIDE/Trabajos de R/Tidyversecurso/data/challenge.rds")
        read_rds("../Desktop/Directorio/CIDE/Trabajos de R/Tidyversecurso/data/challenge.rds")
                
        
        
        #Paquete "Feather"         
        
        library("feather")
        
        write_feather(challenge, "../Desktop/Directorio/CIDE/Trabajos de R/Tidyversecurso/data/challenge.feather")
        read_rds("../Desktop/Directorio/CIDE/Trabajos de R/Tidyversecurso/data/challenge.rds")
        
        
        # Sesion 89: Limpieza de datos
        
        
        
        
        
        