#----------------------------------------------------------------------------------------------------------------------------
cuadradosMedios<-function(n,semilla){
  #Algoritmo cuadrados medios
  
  
  DLongitudSemilla = nchar(semilla)
  #Evaluacion de la longitud de la semilla, en caso que sea menor a 3 no se realizara el proceso del algoritmo
  if(DLongitudSemilla > 3){
    if(n>0){
      print("Los numeros pseudoaleatorios generados son: ")
      #Creacion del ciclo que repetira las operaciones del Algoritmo
      for (i in 1:n) {
        Y = semilla^2
        #Conversion del valor de Y a formato de cadena para realizar la selccion de los numeros del centro
        YCaracter = as.character(Y)
        
        #Se obtiene la longitud del cuadrado
        DLongitudSemillaAlCuadrado = nchar(YCaracter)
        
        #Seleccion de los numeros que se encuentran en el centro
        Dinicio = ((DLongitudSemillaAlCuadrado - DLongitudSemilla)/2)
        
        #Asignacion de los numeros del centro a la variable X con formato de tipo cadena
        X = substr(YCaracter, start = Dinicio+1, stop = Dinicio+DLongitudSemilla)
        
        #Coversion del valor de X a un tipo de dato numerico para realizar el calculo de los numeros pseudoaleatorios restantes
        r=as.numeric(X)
        
        #Asignacion del valor de "r" a la variable "semilla" con el fin de que tome el nuevo valor para poder calcular el cuadrado del numero y repetir el proceso anterior
        semilla=r
        
        #Generacion del numero pseudoaleatorio
        r=r/10^DLongitudSemilla
        
        #Visualizacion del numero pseudoaleatorio en forma de mensaje con un salto de linea
        cat('Numero',i,':\t',r , '\n')
        
      }
      
    }else{
      print("El valor de n debe ser mayor a cero y no debe ser un numero negativo!.")
    }
  }else
    print("El valor de la semilla debe tener minimo 4 digitos.")
}

#----------------------------------------------------------------------------------------------------------------------------
productosMedios<-function(n){
  #se ingresa el valor de las semillas para posteriormente obtener su longitud
  print("Ingrese la primera semilla: ")
  semillaZ = scan(n=1)
  print("Ingrese la segunda semilla: ")
  semillaF = scan(n=1)
 
  DLongitudSemillaZ = nchar(semillaZ)
  DLongitudSemillaF = nchar(semillaF)
  #Se verifica la longitud de las semillas 
  if(DLongitudSemillaZ > 3 && DLongitudSemillaF > 3 && DLongitudSemillaZ ==DLongitudSemillaF){
    if(n>0){
      print("Los numeros pseudoaleatorios generados son: ")
      #Se repite el proceso de calculo del algoritmo mediante una estructura ciclica for la cual finaliza hasta que la variable i sea igual al numero de iteacciones ingresado por el usuario representado por la variable n
      for (i in 1:n) {
        #Se realizan los calculos necesarios para calcular los numeros pseudoaleatorios y despues de convertirlo a caracter por medio as.character se selecciona los D digitos del centro con la funcion substr y se convierte a un valor entero para ser asignado
        Y = semillaZ *semillaF
        YCaracter = as.character(Y)
        DLongitudSemillaAlCuadrado = nchar(YCaracter)
        Dinicio = ((DLongitudSemillaAlCuadrado - DLongitudSemillaZ)/2)
        X = substr(YCaracter, start = Dinicio+1, stop = Dinicio+DLongitudSemillaZ)
        r=as.numeric(X)
        semillaZ=semillaF
        semillaF=r
        r=r/10^DLongitudSemillaZ
        #Se imprimen los  numeros
        cat('Numero',i,':\t',r , '\n')
      }
    }else{
      print("El valor de n debe ser mayor a cero y no debe ser un numero negativo!.")
    }
    
  }else
    print("El valor de la semilla debe tener minimo 4 digitos.")
}
#----------------------------------------------------------------------------------------------------------------------------
multiplicadorConstante<-function(n){
  #Algoritmo de multiplicador constante
  #Se solicitan los valores de la semiula y constante para posteriormente obtener la longitud de cada una
  print("Ingrese el valor de la semilla: ")
  semilla = scan(n=1)
  print("Ingrese el valor de la constante: ")
  constante = scan(n=1)
  
  DLongitudSemilla = nchar(semilla)
  DLongitudConstante = nchar(constante)
  #Se verifica la longitud de la semilla y la constante
  if(DLongitudSemilla > 3 && DLongitudConstante > 3 && DLongitudSemilla ==DLongitudConstante){
    if(n>0){
    #Se repite el proceso de calculo del algoritmo mediante una estructura ciclica for la cual finaliza hasta que la variable i sea igual al numero de iteacciones ingresado por el usuario representado por la variable n
    for (i in 1:n) {
      #Se realizan los calculos necesarios para calcular los numeros pseudoaleatorios y despues de convertirlo a caracter por medio as.character se selecciona los D digitos del centro con la funcion substr y se convierte a un valor entero para ser asignado
      Y=constante*semilla
      YCaracter = as.character(Y)
      DLongitudSemillaObtenida= nchar(YCaracter)
      Dinicio = ((DLongitudSemillaObtenida - DLongitudSemilla)/2)
      X = substr(YCaracter, start = Dinicio+1, stop = Dinicio+DLongitudSemilla)
      r=as.numeric(X)
      semilla=r
      r=r/10^DLongitudSemilla
      #se imprime el numero pseudoaleatorio generado indicando su posicion
      cat('Numero',i,':\t',r , '\n')
          }
      
    }else{
      print("El valor de n debe ser mayor a cero y no debe ser un numero negativo!.")
    } 
  }else
    print("El numero debe tener minimo 4 digitos y ambas deben tener la misma longitud.")
}
#----------------------------------------------------------------------------------------------------------------------------
congruenciaLineal<-function(n){
  #Algoritmo de congruencia lineal
  #Se ingresa los valores de las variables correspondientes
  print("Ingrese el valor de la semilla: ")
  semilla= scan(n=1)
  print("Ingrese el valor de la variable K: ")
  k = scan(n=1)
  print("Ingrese el valor de la variable G: ")
  g = scan(n=1)
  print("Ingrese el valor de la variable C: ")
  c = scan(n=1)
  
  #verificacion de valores enteros para las variables "g" y "k"
  if(g %%1==0 && k%%1==0){
    #verificar que la variable c sea mayor a 0
    if(c >0){
      #verificar que c sea coprimo a m
      if(c%%2==1){
        if(n>0){
          #Se repite el proceso de calculo del algoritmo mediante una estructura ciclica for la cual finaliza hasta que la variable i sea igual al numero de iteacciones ingresado por el usuario representado por la variable n
          for (i in 1:n) {
            #Se calculan los valores de las variables m, a, para despues obtener el modulo de esa operacion respecto a posteriormente se calcula el valor de xi e
            m=2^g
            a=1+4*k
            Y=(a*semilla+c)%% (m)
            r=Y/(m-1)
            semilla=Y
            #se imprime el numero pseudoaleatorio generado indicando su posicion
            cat('Numero',i,':\t',r , '\n')
          }
        }else{
          print("El valor de n debe ser mayor a cero y no debe ser un numero negativo!.")
        }
      }else{
        print("El valor de c debe ser distinto a un numero par o cero")   
      }
    }else {
      print("El valor de c debe ser mayor a 0")
    }
  } else {
    print("Los valores DEBEN SER ENTEROS")
  }
}
#----------------------------------------------------------------------------------------------------------------------------
congruenciaCuadratica<-function(n){
  #Algoritmo de congruencia cuadratica
  #Se ingresa los valores de las variables correspondientes
  print("Ingrese el valor de la semilla: ")
  semilla= scan(n=1)
  print("Ingrese el valor de la variable 'a': ")
  a = scan(n=1)
  print("Ingrese el valor de la variable 'b': ")
  b = scan(n=1)
  print("Ingrese el valor de la variable 'c': ")
  c = scan(n=1)
  print("Ingrese el valor de la variable 'g': ")
  g= scan(n=1)
  # a, b,c,g,
  #Se verifica las condiciones que deben cumplir las variables a,c,g,b, y n para que el ciclo de vida del algoritmo sea optimo
  if(a%%2==0){
    if(c%%2==1){
      if(g%%1==0){
        if((b-1)%%4==1){
          if(n>0){
            #Se repite el proceso de calculo del algoritmo mediante una estructura ciclica for la cual finaliza hasta que la variable i sea igual al numero de iteacciones ingresado por el usuario representado por la variable n
            for (i in 1:n) {
              #se realizan los calculos involucrados en el algoritmo mediante el calculos de las variables y sus respectivas operaciones
              m=2^g
              Y=(a*semilla^2+b*semilla+c)%% (m)
              r=Y/(m-1)
              semilla=Y
              #se imprime el numero pseudoaleatorio generado indicando su posicion
              cat('Numero',i,':\t',r , '\n')
            } 
          }else{
            print("El valor de n debe ser mayor a cero y no debe ser un numero negativo!.")
          }
        }else{
          cat('EL valor de la variable "b" debe cumplir la condicion de: (b-1) mod 4==1')
        }
      }else{
        cat('EL valor de la variable "g" debe ser un numero entero')
      }
    }else{
      cat('EL valor de la variable "c" debe ser un numero impar')
    }
  }else{
    cat('EL valor de la variable "a" debe ser un numero par')
  }
}
#----------------------------------------------------------------------------------------------------------------------------
BBS<-function(n){
  #Algoritmo de Blum, Blum y Shub BBS
  #Se solicitan los valores de la semila y la variable g
  print("Ingrese el valor de la semilla: ")
  semilla= scan(n=1)
  print("Ingrese el valor de la varioable 'g': ")
  g= scan(n=1)
  #se verifican los valores de las variables g y n donde estas deben ser mayores a cero
  if(g>0){
    if(n>0){
      #Se repite el proceso de calculo del algoritmo mediante una estructura ciclica for la cual finaliza hasta que la variable i sea igual al numero de iteacciones ingresado por el usuario representado por la variable n
      print("Los numeros pseudoaleatorios generados son: ")
      for (i in 1:n) {
        m=2^g
        Y=(semilla^2)%% (m)
        r=Y/(m-1)
        semilla=Y
        #se imprime el numero pseudoaleatorio generado indicando su posicion
        cat('Numero',i,':\t',r , '\n')
      }
      
    }else{
      print("El valor de n debe ser mayor a cero y no debe ser un numero negativo!.")
    }
  }else{
    cat('El valor de la variable g debe ser mayor a 0')
  }
}
#----------------------------------------------------------------------------------------------------------------------------
bandera=TRUE
#se muestra un menu utilizando una estructura ciclica while en la que se definen los diferentes tipos de algortimos desarrollados 
while(bandera){
  cat('___________________________________________________\n')
  cat('Algoritmos generadores de numeros pseudoaleatorios \n')
  cat('___________________________________________________\n')
  cat('Menu de opciones:\n')
  cat('\t 1.- Algoritmo de cuadrados medios.\n')
  cat('\t 2.- Algoritmo de productos medios.\n')
  cat('\t 3.- Algoritmo de multiplicador constante.\n')      
  cat('\t 4.- Algoritmo congruencial lineal.\n')
  cat('\t 5.- Algoritmo congruencial cuadrático.\n')
  cat('\t 6.- Algoritmo de Blum, Blum y Shub (BBS).\n')
  cat('\t 7.- Salir. \n')
  #Solicitud al usuario para que ingrese algo valor del menu para generar los numeros
  cat('A continuación seleccione un algoritmo para generar los numeros pseudoaleatorios ingresando una opcion entre 1-6 \n')
  opcion = scan(n=1)
  #Se establece una condicion en cada caso de uso de cada algoritmo representado por una opcion entre 1 y 6 dependiendo del tipo de algoritmo seleccionado son los parametros solicitados
  #Todas las funciones comparten el parametro de n, a excepcion de la semilla que en algunos casos pueden ser dos o una constante
  if(opcion==1){
    cat('Ingrese la cantidad de numeros aleatorios que desea generar\n')
    n=scan(n=1)
    cat('Ingrese el valor de la semilla\n')
    semilla=scan(n=1)
     cuadradosMedios(n,semilla)
  }else if(opcion==2){
    cat('Ingrese la cantidad de numeros aleatorios que desea generar\n')
    n=scan(n=1)
    productosMedios(n)
  }else if(opcion==3){
    cat('Ingrese la cantidad de numeros aleatorios que desea generar\n')
    n=scan(n=1)
    multiplicadorConstante(n)
  }else if(opcion==4){
    cat('Ingrese la cantidad de numeros aleatorios que desea generar\n')
    n=scan(n=1)
    congruenciaLineal(n)
  }else if(opcion==5){
    cat('Ingrese la cantidad de numeros aleatorios que desea generar\n')
    n=scan(n=1)
    congruenciaCuadratica(n)
  }else if(opcion==6){
    cat('Ingrese la cantidad de numeros aleatorios que desea generar\n')
    n=scan(n=1)
    BBS(n)
  }else if(opcion==7){
    cat('Bye!.\n')
    bandera=FALSE 
  }else {
    cat('Ingrese una opción valida!.\n')
    bandera=FALSE
  }
  
}
