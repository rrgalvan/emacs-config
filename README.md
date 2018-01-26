Configuración "sencilla" de Emacs
=================================

Ficheros de configuración de Emacs orientados a que

* el uso de este editor resulte sencillo para el usuario
* y a la vez incluya todas las características que hacen de Emacs un potente entorno de computación científica.

Instalación
---------------
0. Instalar emacs y emacs-goodies-el
sudo apt-get intall emacs emacs-goodies-el 
1. Asegurarse de que en la carpeta $HOME no existe el archivo regular *.emacs* ni el directorio *.emacs.d* (donde se almacena la configuración de emacs). Por ejemplo, se pueden borrar directamente (por supuesto, si se desea guardar la configuración previa, se debe hacer una copia antes de borrar):

		$ rm -rf .emacs .emacs.d

2. En la carpeta $HOME, clonar el repositorio

        $ git clone <dirección ssh o https de este repositorio>

3. (Opcional) Para mejorar el tipo de letras, instalar el paquete emacs *fonts-inconsolata*:

		$ sudo apt-get install fonts-inconsolata
La próxima vez que se arranque Emacs, se descargarán automáticamente de internet algunos paquetes y se mostrarán varios mensajes de aviso (el resto de las veces, estos mensajes no aparecerán).

4. (Modo Python) Para optimizar la integración de Emacs con Python, instalar el paquete Python "virtualenv":

        $ sudo pip install virtualenv

Existen muchos recursos en internet para una introducción rápida a Emacs, por ejemplo [éste, muy completo](http://es.tldp.org/Tutoriales/doc-tutorial-emacs/intro_emacs.pdf) y [éste, muy breve](http://exal.0x2.org/emacs/emacs.html)
=======
# Emacs Configuration

Jessica Hamrick  
jhamrick@berkeley.edu

**Version**: GNU Emacs 24.3.1 (x86_64-apple-darwin12.4.0, NS apple-appkit-1187.39) of 2013-06-27

## Emacs plugins

The following plugins are required for this configuration. Each plugin
lists the other included plugins that it depends on.

### Included as submodules

To install these plugins that are include as submodules, you will need
to run `git submodule init; git submodule update`.

* [Solarized Color Theme](https://github.com/sellout/emacs-color-theme-solarized) (`.emacs.d/plugins/color-theme-solarized`)
* [el-get](https://github.com/dimitri/el-get) (`.emacs.d/plugins/el-get`)
* [Emacs IPython Notebook](https://github.com/tkf/emacs-ipython-notebook) (`.emacs.d/plugins/emacs-ipython-notebook`)
	* Jedi
	* Markdown Mode
	* MuMaMo (nxhtml)
	* Request
	* Websocket
* [Magit](https://github.com/magit/magit) (`.emacs.d/plugins/magit`)
* [Markdown Mode](http://jblevins.org/projects/markdown-mode/) (`.emacs.d/plugins/markdown-mode`)
* [nxhtml](https://github.com/emacsmirror/nxhtml) (`.emacs.d/plugins/nxhtml`)
* [Request](https://github.com/tkf/emacs-request) (`.emacs.d/plugins/request`)
* [SCSS Mode](https://github.com/antonj/scss-mode/) (`.emacs.d/plugins/scss-mode`)
* [Websocket](https://github.com/ahyatt/emacs-websocket) (`.emacs.d/plugins/websocket`)

### Install using el-get

These plugins should be installed using `el-get` (which is included as
a submodule). To install these, open emacs and run `M-x el-get-install
<plugin>`.

* [Auto Complete](https://github.com/auto-complete/auto-complete)
* [Jedi](https://github.com/tkf/emacs-jedi)
* [Popup](https://github.com/auto-complete/popup-el)

### Manual install

These plugins must be manually downloaded and saved to the path
indicated in the parentheses.

* [AUCTex 11.87](http://www.gnu.org/software/auctex/download.html) (`.emacs.d/plugins/auctex`)
* [Pydoc Info 0.2](https://bitbucket.org/jonwaltman/pydoc-info) (`.emacs.d/plugins/pydoc-info-0.2`)
* [MATLAB CVS version 2013-04-02](http://matlab-emacs.sourceforge.net/) (`.emacs.d/plugins/matlab`)

## Non-Emacs dependencies

* Python 2.7.4
* IPython 1.0dev
