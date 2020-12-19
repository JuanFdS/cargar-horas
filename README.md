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
- Carpeta en la cual persistir los registros cargados. Cada vez que el programa corre agrega un .json ahí con el timestamp de cuando se corrió el programa. Cuando le pedís las horas va a darte las de todos los archivos de registros que hay en la carpeta.
- Puerto en el cual va a estar escuchando el programa. Este parámetro es opcional, el default es `8080`.

Ponele que el binario se llama `registrarHoras`, lo usarías como:
`./registrarHoras ~/Desktop/horas.json 8080`

### Guardando y mostrando eventos

Se pueden mandar instrucciones por websocket al puerto en el que lo hayas configurado.
Estás son las cosas que puede recibir el programa por ahora:
- {"accion": "cargar"} -> Abre una ventana en la cual escribir que estás haciendo, una vez escribís y apretas enter se registra.
Ejemplo:
- {"accion": "mostrar"} -> Te devuelve todos los registros cargados hasta ahora en orden cronológico y agrupados por día.
- {"accion":"registrar", "registro": { "evento": "JxP", "tag": "Gestion interna", "metadata": [{"tipo":"jardinereade", "info":"nico"}}} -> Registra un evento con nombre "JxP", con "Gestion interna" como tag y con la metadata de que el jardinereado de esa reunión fue nico.

Cada evento tiene un nombre y una lista de metadata que son objetos con una clave y un valor.

## En mi compu a la ventana para cargar no se le pone el foco automaticamente :(

Sep, acá hice trampa porque no encontré como hacer esas cosas con gtk aun, lo que hice fue instalar devilspie2 y tener un script de devilspie2 que le ponga el foco a esa ventana:
```
if (get_window_name() == "cargar-horas-exe") then
  focus_window()
end
```


