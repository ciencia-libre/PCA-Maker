*PCA Maker*
================

Una aplicación Shiny para realizar análisis de componentes principales a partir de datos en formato TSV y CSV.

Puedes utilizar *PCA Maker* de dos formas: 

#### 1

Simplemente descarga este repositorio de *GitHub*. Una vez en la carpeta principal de *PCA Maker*, da doble clic en el archivo *PCA-Maker.Rproj* y posteriormente escribe en la consola de *R* (es necesario el paquete *devtools*): 

`devtools::laod_all(".")`

y finalmente:

`PCAMaker()`

#### 2

Instala la aplicación como un paquete con la siguiente línea de codigo:  

`devtools::install_github("___/PCA-Maker")`

entonces:

`library(PCAMaker)`

y finalmente:

`PCAMaker()`  

Para hacer lo último, también puedes correr el código en el archivo *app.R* en la carpeta principal de *PCA Maker*.

Juan Pablo Carreón Hidalgo

<jpch_26@outlook.com>  

[GNU General Public License v3.0](https://www.gnu.org/licenses/gpl-3.0.html)
