# cargar-horas

Primera versión de este coso para tratar de llevar registro de lo que hago en el día.

# Como instalar

## Prerrequisitos

El proyecto se compila con stack
https://docs.haskellstack.org/en/stable/README/

Y usa gtk así que seguro hay todo un bardo de dependencias que vas a tener que instalar. #TODO: completar esto

## Instalando

Se puede obtener un ejecutable con `stack install`, esto va a buildear el proyecto y copiar el binario a una carpeta que se va a imprimir en la consola.

## Usandolo

### Correr el programa

Una vez tenés el binario, al correrlo recibe dos parámetros:
- Archivo en el cual persistir los registros cargados, tiene que tener una lista de JSONs. Podes empezar con un archivo con lista vacía (`[]`).
- Puerto en el cual va a estar escuchando el programa. Este parámetro es opcional, el default es `8080`.

Ponele que el binario se llama `registrarHoras`, lo usarías como:
`./registrarHoras ~/Desktop/horas.json 8080`

### Guardando y mostrando eventos

Se pueden mandar instrucciones por socket al puerto en el que lo hayas configurado.
Estás son las cosas que puede recibir el programa por ahora:
- "cargar" -> Abre una ventana en la cual escribir que estás haciendo, una vez escribís y apretas enter se registra.
- "mostrar" -> Te devuelve todos los registros cargados hasta ahora en orden cronológico y agrupados por día.
- "commit|EL_REPO|LA_BRANCH|EL MENSAJE DE COMMIT" -> Registra un evento de commit, que va a tener como metadata el repo, branch y mensaje de commit.
- "registrar|NOMBRE DEL EVENTO|TIPO_DE_METADATA:VALOR|OTRO_TIPO_DE_METADATA:VALOR" -> Registra un evento con nombre y todas las metadatas que se quieran pasar (pueden ser 0)

NOTA: ahora siempre está tratando de obtener cierta metadata default que son las branches de dos repos que uso, eso esta hardcodeadisimo lo tengo que sacar.

Cada evento tiene un nombre y una lista de metadata que son objetos con una clave y un valor.

