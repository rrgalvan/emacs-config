Configuración "sencilla" de Emacs
=================================

Ficheros de configuración de Emacs orientados a que

* el uso de este editor resulte sencillo para el usuario
* y a la vez incluya todas las características que hacen de Emacs un potente entorno de computación científica.

Instalación
---------------
1. Asegurarse de que en la carpeta $HOME no existe el archivo regular *.emacs* ni el directorio *.emacs.d* (donde se almacena la configuración de emacs). Por ejemplo, se pueden borrar directamente (por supuesto, si se desea guardar la configuración previa, se debe hacer una copia antes de borrar):

		$ rm -rf .emacs .emacs.d

2. En la carpeta $HOME, clonar el repositorio

        $ git clone <dirección ssh o https de este repositorio>

3. (Opcional) Para mejorar el tipo de letras, instalar el paquete emacs *fonts-inconsolata*:

		$ sudo apt-get install fonts-inconsolata

La próxima vez que se arranque Emacs, se descargarán automáticamente de internet algunos paquetes y se mostrarán varios mensajes de aviso (el resto de las veces, estos mensajes no aparecerán).

4. (Modo Python) Para optimizar la integración de Emacs con Python, instalar el paquete Python "virtualenv":

        $ sudo pip install virtualenv

Existen muchos recursos en internet para una introducción rápida a internet, por ejemplo [éste, muy completo](http://es.tldp.org/Tutoriales/doc-tutorial-emacs/intro_emacs.pdf) y [éste, muy breve](http://exal.0x2.org/emacs/emacs.html)