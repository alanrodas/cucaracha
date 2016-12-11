Lenguaje Cucaracha
==================

El lenguaje cucaracha es una simple implementación de un lenguaje de
programación desarrollado para la materia de Parseo y Generación de Código de
la Universidad Nacional de Quilmes.

Esta implementación está realizada en Haskell utilizado Alex y Happy
como lexer y parser. Además compila a código x86-64 para arquitecturas Intel
en sistemas Linux. Puede utilizar Haskell Stack para correrlo.

Compilando el código
--------------------

Puede utilizar GHC y la plataforma haskell para compilar, pero sugerimos utilizar
Haskell Stack compilar la aplicación. Siga los siguientes pasos

1. [Instalar Stack](https://docs.haskellstack.org/en/stable/README/)
2. Clonar este repositorio en una carpeta en su equipo
3. Abrir una terminal en dicha carpeta y ejecutar `stack setup`
4. En la misma terminal ejecutar `stack build alex`
5. En la misma terminal ejecutar `stack build happy`

Su sistema habrá descargado todas las dependencias necesarias para compilar
el código fuente. En la misma terminal puede ejecutar ahora
```
stack build
```
Esto compilará el código fuente y generará un archivo ejecutable para su
plataforma. Recuerde que si realiza alguna modificación al código, deberá
volver a ejecutar este comando para el correcto funcionamiento del mismo.

Una vez compilado, deberá ver una nueva carpeta bajo el nombre de
`.stack-work` en la carpeta del proyecto. Allí podrá encontrar el archivo
ejecutable generado en la ruta
`.stack-work/dist/<architecture>/Cabal-<cabal-version>/build/cucaracha-exe/cucaracha-exe`
donde architecture dependerá del procesador y sistema operativo sobre el cual
se encuentre ejecutando (ej. x86_64-osx) y <cabal-version> dependerá de la versión
de cabal que este ejecutando (que a su vez depende de la versión de Stack instalada).

Corriendo la aplicación
-----------------------
Una vez construida la aplicación puede utilizar el binario generado directamente,
o puede continuar utilizando stack para mayor comodidad.

La aplicación espera una serie de argumentos para poder ejecutarse correctamente.
Puede ver los argumentos esperados ejecutando el programa con el flag `-?` o `--help`

Utilice stack de la siguiente forma para ver la ayuda de la aplicación:

```
stack exec cucaracha-exe -- -?
```

El primer doble guion indica que los argumentos deben ser pasados a la aplicación
ejecutada en lugar de a stack, luego de este simbolo puede colocar tantos parametros
de la aplicación como le parezca.

Los posibles flags que puede dar a la aplicación son:

* -? --help          (Muestra el mensaje de ayuda)
* -V --version       (Muestra la versión de la aplicación)
* --numeric-version  (Muestra la versión numérica de la aplicación)
* -t --tokens        (Muestra el resultado de la tokenización en la consola)
* -a --ast           (Muestra el AST en formato texto en la consola)
* -c --check         (Muestra el resultado del chequeo de tipos en la consola)
* -e --assembly      (Produce el archivo con código ensamblador)
* -C --compile       (Produce un archivo ejecutable)
* -X --execute       (Ejecuta el programa)

Puede combinar estos flags como desee, por ejemplo, si desea solamente correr
el programa, pero no le interesa generar un archivo ejecutable, puede usar
solamente `-X`, en cambio, si desea ver el AST, y generar el archivo en codigo
ensamblador y luego correrlo, puede utilizar `-aeX`.

Finalmente hay dos parametros que son de suma relevancia y que esperan un valor
como argumento, el nombre del archivo de entrada y del de salida.

* -f --file\[=\[input\]\]  (El archivo de entrada, defaultea a program.cuca)
* -o --out\[=\[output\]\]  (El archivo de salida, defaultea a program)


Corriendo los tests de la aplicación
------------------------------------

El parser, el chequeador de tipos y el compilador tienen una serie de tests
que verifican su correcto funcionamiento. Si desea correr los tests
puede ejecutar los mismos mediante el comando:

```
stack test
```

Esto evaluará los tests utilizando HSpec y verificará el correcto funcionamiento
de las distintas partes del sistema.
