<center>
<div id="miDiv">
    <img src="https://www.coordinador.cl/wp-content/themes/coordinador-electrico/assets/img/logo.svg" width="15%" height="15%">
</div>
</center>
<hr>

<h1 align='center'> <b>PLP-Fortran versión 6.3 Baterias </b></h1>

<hr>

<h2>Introducción</h2>
PLP-Fortran consiste en el software implementado en Fortran y C++ que permite la coordinación hidrotérmica a través del algoritmo SDDP modificado, incluyendo características únicas como los convenios de riego (Cuenca del Laja y Cuencua del Maule) y además de modificaciones en rendimientos según cotas.

<h2>Requisitos </h2>
Para la ejecución de este software es necesaria su compilación, para esto se recomienda seguir el <a href="https://www.coordinador.cl/wp-content/uploads/2019/08/Minuta-GO-N%C2%BA04-2019-Manual-de-Instalaci%C3%B3n-PLPv5.0.pdf">siguiente manual</a>.

En particular, se recomienda la instalación de las siguientes librerias:
~~~bash 
yum groupinstall "Development Tools"
yum install zlib-devel lapack-devel time screen dos2unix
~~~
además de la transformación a formato unix cualquier archivo traspasado en la máquina donde se ejecutará PLP a través del siguiente comando en los directorios importados
~~~bash
find . -type f -exec dos2unix {} \; 
~~~
<h2>Modificaciones con la versión anterior</h2>

<h3> Cambios de la versión 6.0 a la 6.1 en PLP </h3>

<h4> Archivos modificados: <h4>

*   osicalls.cpp
*   plp-rendim.f
*   genpdreserva.f
*   plpmatvar.f
*   parreserva.f
*   plp-pdmatld.f


<h4> Detalle de las modificaciones: <h4>

*   osicalls.cpp:  
Se modifica la forma de realizar cortes de factibilidad, ya que al modificar las cotas de las variables $v^+,v^-$, se modifican los valores de la variable dual sujeta a la restricción <br> 
$$ x_t + v^+ - v^-  = b_t + \overline{x_{t-1}}$$
Por lo cual afecta al rayo calculado $\pi$, el cual debe tomar los valores $-1, 1$. Así, se fuerza el valor del rayo de factibilidad dependiendo del signo que tome, forzandolo de la siguiente manera
$$\pi = sgn(\pi)$$
así se agregan las lineas en la línea 730 del archivo

```C
        if (ray[i] <0){
          ray[i]=-1
        }
        if (ray[i]> 0){
          ray[i]=1
        }
```
* plp-rendim.f: <br>
Se modifica plp-rendim.f según lo de la linea 715 de genpdreserva.f, ya que el rendimiento entraría en las restricciones que dependen según cada zona respectivamente.

* genpdreserva.f, parreserva.f y plpmatvar.f: <br>
Se modifica la modelación de reservas, donde se modifica el archivo genpdreserva.f eliminando restricciones, el archivo parreserva.f, eliminando variables y restricciones que no se usan, y se modifica plpmatvar.f donde se elimina un parámetro de entrada de la función genpdreserva.f.
Se modifica el parámetro de  número de variables y restricciones.

* plp-pdmatld.f: <br>
Se modifica el archivo pdmatld.f linea 170, donde se realiza un ciclo for modificando el índice del lado derecho, ya que ahora existen más restricciones.





---

<h3> Cambios versión 6.1 realizados 19-10-2023 </h3>

<h4> Archivos modificados: </h4>

*   plp-rendim.f
*   genpdreserva.f
*   parreserva.f
*   plp-pdmatld.f

<h4> Detalles de las modificaciones </h4>

* plp-rendim.f: <br>
Se modifica plp-rendim.f línea 182, se agrega la enumeración del lado derecho según lo de la linea 733 de genpdreserva.f, ya que el rendimiento entraría en las restricciones para cada central, según corresponda

* genpdreserva.f, parreserva.f: <br>
Se agrega nueva restricción de en la modelación de reservas del estilo 

$$RC_c P_{c}^b  - \sum_{s \in RES^-,z \in Zonas} R_{s,z}^b \geq 0  \quad \forall c \in G \forall b \in BL \quad (0)$$

donde $RES^-$ corresponde al conjunto de servicios de bajada.
Básicamente esta restricción permite que el margen de la reserva de la central de bajada se comparta, es decir, si se modela la inercia y el CF de bajada, no utilice el gap que utiliza actualmente la inercia para dar CF de bajada, sino que utilice un nuevo gap.

Con esto, se modifica el archivo genpdreserva.f, agregando la restricción en la línea 733, el archivo parreserva.f, agregando los parámetros para realizar esta restricción, y se modifica plpmatvar.f.

* plp-pdmatld.f: <br>
Se modifica el archivo pdmatld.f linea 170, donde se realiza un ciclo for modificando el índice del lado derecho, ya que ahora existen más restricciones.


---

<h3> Cambios versión 6.2 realizados 24-10-2023 </h3>

<h4> Archivos modificados: </h4>

*   plp-rendim.f
*   genpdreserva.f
*   parreserva.f
*   pdmatinv.f

<h4> Detalles de las modificaciones </h4>

* plp-rendim.f: <br>
Se agrega línea 187 plp-rendim.f , se agrega la enumeración del lado derecho según lo de la linea 837 de genpdreserva.f, ya que el rendimiento estaría multiplicando la nueva variable de caudal utilizado en reserva de subida.

* genpdreserva.f, parreserva.f: <br>
Se agrega nuevo término en la ecuación de balance del estilo

$$qe + qg + \sum_{z \in Zonas, s \in RES^+} qrp_{z} + ... = Q$$

para las centrales con la ecuación de balance de aguas (Embalses y en Serie que hagan reservas) donde la nueva variable $qrp$ viene dada según la siguiente definición

$$ RC_c qrp_c^b = \sum_{z \in Zona, s \in RES^+} FACTOR_z*H_{b}rp_{z,s}^b \quad \forall b \in BL \forall c \in (G_{emb}\cup G_{ser})\cap G_{RES} \quad (1)$$ 

donde $RES^+$ corresponde al conjunto de servicios de subida.

Básicamente esta restricción permite que las centrales hidraulicas no tengan un margen infinito para dar servicios de subida, y a su vez utilicen agua para dar servicios de subida como se ve en la operación real.

Con esto, se modifica el archivo genpdreserva.f, agregando una nueva variable y agregando una nueva restricción entre las lineas 822 a 877. Esta modificación viene de la mano con agregar los términos al archivo parreserva.f, modificando la estructura con nuevos índices.

Cabe destacar que estas modificaciones de la función de genpdreserva.f, se utilizó nuevas variables de entrada, por lo que se modifica el archivo pdmatinv.f para agregarlos.

Además se agrega la lectura de dos parámetros de entrada: <br>
* FactorCaudal:= Corresponde al parámetro FACTOR utilizado en (1), se agrega en la línea de variables de escacez de plpcnfres.dat 
* FInercia:= Corresponde a la flag que indica que reserva será de inercia, se agrega en la linea de Requerimientos de Zona del archivo plpcnfres.dat. Esta flag modifica la restricción (0), de manera que quede
$$RC_c P_{c}^b  - \sum_{s \in RES^-,z \in Zonas_{NI}} R_{s,z}^b - \sum_{z \in Zonas_{I}} R_{CPF^-,z} \geq 0  \quad \forall c \in G \forall b \in BL \quad (0) $$
Donde $Zonas_{NI}$ corresponden a las zonas marcadas con la FInercia como False (F), es decir zonas que no son de inercia, y $Zonas_{I}$ son las zonas marcadas con FInercia como True (T), por lo que solamente cuentan el $CPF^-$ para el cálculo del gap del servicio de bajada descrito en la restricción (0).

---

<h3> Cambios versión 6.2 realizados 09-11-2023 y 12-12-2023 </h3>
Se agregan las variables para la modificación de los derechos del Laja, agregando de esta manera el consumo/no consumo de agua por los servicios de subida/bajada en las ecuaciones de balance de derechos, además de la modificación del balance de agua en las ecuaciones de embalses

<h4>Archivos modificados</h4>
En general se realizan grandes modificaciones a los siguientes archivos, para mayor detalle revisar historial de commits:

*   genpdreserva.f
*   plp-pdmatld.f  
*   plp-rendim.f  
*   genpdlajam.f  
*   parreserva.f  

---

<h3> Cambies versión 6.3 realizados al 15-04-2024 </h3>

Se agrega el objeto <b> Bateria </b>. El cual funciona con un balance de energía entre bloques de cada etapa, donde una central le puede inyectar energía, y este decide cuando descargar su energía correspondiente en cada bloque, según lo que sea más económico. <br>
Se agrega la lectura de un nuevo archivo de entrada opcional <b> centipo.csv </b> el que nos permitirá modificar la columna CenTip del archivo de salida plpcen.csv, para poder clasificar de manera precisa al momento de manejar los datos de la salida PLP.

<h4> Archivos modificados </h4>
Se agregaron nuevos archivos, los cuales serían:

* genpdbaterias.f
* parbaterias.f
* leecentipo.f

Y se realizaron los cambios a los siguientes archivos para poder agregar la modelación al problema


* pardims.f  
* leecnfce.f  
* extclave.f  
* defprbpd.f  
* extroper.f  
* genmatpd.f  
* genpdreserva.f
* getopts.f  
* nomcnumc.f  
* pdmatinv.f  
* pdmatvar.f  
* plp-gradat.f  
* plp-gdbdcen.f
* plp-gdbdcen2.f
* plp-main.F  
* plp-pdmatld.f  
* plp-version.f  
* plpmod.f  
* CMakeLists.txt  

<h4> Detalles de las modificaciones </h4>
<b>Archivos Nuevos</b>:

* genpdbaterias.f, parbaterias.f: <br>
genpdbaterias.f: 
Este archivo contiene todas las funciones que permiten tanto la lectura de los archivos relacionado con la modelación de Baterias, como las funciones que modifican los problemas lineales y agregan las restricciones y variables necesarias para la modelación de las baterias correspondientes. <br>
parbaterias.f: 
Este archivo contiene los parametros de la estructura ParBaterias que se utiliza dentro del código, guardando así la información relacionada con la mdoelación de baterias, tanto los parámetros físicos que se utilizarán en los problemas lineales, como también los índices correspondientes para la extracción y manejo de variables dentro del código.<br>
La nueva modelación de baterias se describe a continuación:<br>
$$
ET_B= \sum_{b \in BL, i \in G_{iny,B}} FPC_{B,i} EC_i^b \quad \forall B \in G_{BAT} \\
ED_B^b= \dfrac{H_{tb}}{FPD_B} g_{B}^b \quad \forall B \in G_{BAT} \\
SoCf^1_B= ET_B - ED_{B}^1 \quad \forall B \in G_{BAT} \\
SoCf^b_{B}= SoCf^{b-1}_{B} - ED_{B}^b \quad \forall B \in G_{BAT} \forall b >1 \\
SoCf_{B}^T= 0 \quad \forall B \in G_{BAT} \\
\underline{g_i^b} \leq g_i^b + \dfrac{1}{H_{tb}} EC_i^b \leq \overline{g_i}^b \quad \forall B \in G_{BAT} \forall i \in G_{iny,B} \forall b \in BL \\
\dfrac{\sum_{v \in BL}H_{tv} }{24} \underline{SoCf^b_B} \leq SoCf^b_B \leq \dfrac{\sum_{v \in BL}H_{tv}}{24} \overline{SoCf_{B}^b} \quad \forall B \in G_{BAT} \forall b \in BL \\
0 \leq ET_{B} \leq \dfrac{\sum_{v \in BL}H_{tv}}{24} \overline{SoCf_{B}^1} \quad \forall B \in G_{BAT}
$$

Para mayor detalle del significado de las restricciones, variables y parámetros revisar el documento de modelación de PLP
* leecentipo.f: <br>
Este archivo nos permite leer el archivo opcional centipo.csv, el cual modificará la columna CenTip del archivo de salida plpcen.csv, incluyendo el guardar los parámetros necesarios para esta modificación.

<b>Archivos modificados</b>:

* pardims.f:
Se agrega el label de que viene del archivo centipo.csv para modificar la salida plpcen.csv

* leecnfce.f:
Se modifica la lectura del archivo de entrada plpcnfce.dat esto para considerar los nuevos objetos Baterias, los cuales se ingresan como baterias. Para esto se lee el contador, y se organiza de otra manera el archivo plpcnfce.dat. Para mayor información leer el documento que describe los archivos de entrada de PLP.

* extclave.f: 
Se agrega la nueva clave PCenTipMod, que nos indica si la central correspondiente tiene su label de CenTip modificada por el archivo centipo.csv 

* defprbpd.f:
Se agrega la Flag de modelación de Baterías y los parámetros de Baterias en las funciones correspondientes.

* extroper.f:
Se extraen los resultados de las variables de la simulación correspondientes a la modelación de baterias, el valor dual de la restricción de SoC y además se modifican los contadores de filas y columnas según corresponde.

* genmatpd.f:
Se agrega los contadores de filas y columnas a la matriz de restricciones según la cantidad de baterias que se tengan.

* genpdreserva.f:
Se modifica los contadores de filas y columnas para que calcen con la nueva modelación de baterias correspondiente.

* getopts.f:
Se modifica y agrega la Flag de Baterias, para considerarla si es que se cuenta con el archivo o no.

* nomcnumc.f:
Se comenta y describen los argumentos de la función

* pdmatinv.f:
Se agregan las funciones correspondientes a la modelación de baterias, tanto las restricciones por bloques o por etapas.

* pdmatvar.f:
Se agregan las funciones correspondientes a la modelación de baterias, tanto los valores de las FO como las cotas correspondientes a las nuevas variables.

* plp-gradat.f:
Se agrega la sección para grabar los archivos de salida de baterias, el cual corresponde al archivo plpbat.csv

* plp-gdbdcen.f, plp-gdbdcen2.f: 
Se agrega al texto que se verifica la nueva clave de baterias y además la clave para las centrales modificadas con el archivo centipo.csv

* plp-main.F:
Se agregan las variables relacionadas con baterias, la lectura de los nuevos archivos de entrada, y además de la lectura del archivo centipo.csv

* plp-pdmatld.f:
Se modifican los lados derechos de las restricciones correspondientes a la modelación de baterias.

* plp-version.f:
Se modifica el nombre a la versión

* plpmod.f:
Se incluye la lectura de los nuevos archivos de parámetros de baterias.

* CMakeLists.txt:
Se incluyen los nuevos archivos al momento de la compilación y generación del binario PLP.


<h2>Agradecimientos y comentarios</h2>

Agradecimientos a la historia de gente que ha modificado el código y documentado sus modificaciones, dentro de lo criptico que es el código, con tiempo se puede ir entendiendo cada vez más los rincones pantanosos de cada linea.

En futuras modificaciones, esta el desafío de liberar memoria entre cada iteración de PLP, y agregar método de convergencia exacta (revisar <a href=https://www.researchgate.net/profile/Vincent-Leclere/publication/325366468_Exact_converging_bounds_for_Stochastic_Dual_Dynamic_Programming_via_Fenchel_duality/links/5b0806610f7e9b1ed7f2d9a7/Exact-converging-bounds-for-Stochastic-Dual-Dynamic-Programming-via-Fenchel-duality.pdf>Leclere, V., Carpentier, P., Chancelier, J. P., Lenoir, A., & Pacaud, F. (2020). Exact converging bounds for stochastic dual dynamic programming via fenchel duality. SIAM Journal on Optimization, 30(2), 1223-1250.</a>)

:raised_hands: Agradecidos por tod@s los que colaboran! :raised_hands: