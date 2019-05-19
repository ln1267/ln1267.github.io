---
title: tt
layout: post
author: ln1267
permalink: /anaconda-and-jupyter-notebook/
tags:
- test
source-id: 1nND8DGESaNS7K5K6DBbMw2UQt7iwQTke4QpvDYg9xQ0
published: true
---
# **ANACONDA AND JUPYTER NOTEBOOK**

30 Mar 2018

[ [Python](https://ln1267.github.io/tag/Python)[ ](https://ln1267.github.io/tag/Python) [Programming](https://ln1267.github.io/tag/Programming)[ ](https://ln1267.github.io/tag/Programming) [Jupyter](https://ln1267.github.io/tag/Jupyter)[ ](https://ln1267.github.io/tag/Jupyter) ]

*This is a guide for installing Anaconda and Jupyter Notebook*

## **INSTALL AND CONFIGURE ANACONDA**

### **INSTALL ANACONDA**

* Anaconda can be downloaded from [Anaconda's website](https://www.continuum.io/downloads#_unix). It has both [Linux](http://repo.continuum.io/archive/Anaconda3-4.0.0-Linux-x86_64.sh) and [Windows versions](http://repo.continuum.io/archive/Anaconda3-4.0.0-Windows-x86_64.exe)

* install Anaconda

<table>
  <tr>
    <td>bash *.sh</td>
  </tr>
</table>


After installation, set the default python3 version by adding the following code to *".bashrc"*. Then restart the connection. The default *“.bashrc”* will be changed for Anaconda

<table>
  <tr>
    <td># added by Anaconda3 4.0.0 installerexport PATH="/home/ubuntu/anaconda3/bin:$PATH"</td>
  </tr>
</table>


<table>
  <tr>
    <td># install modulesconda list # check installed modulesconda install netCDF4</td>
  </tr>
</table>


<table>
  <tr>
    <td># updateconda update anaconda</td>
  </tr>
</table>


<table>
  <tr>
    <td># create an environmentconda create -n py35 python=3.5 anaconda# activate and use the created environmentsource activate py35# deactivate an environmentsource deactivate py35</td>
  </tr>
</table>


## **JUPYTER NOTEBOOK**

### **START JUPYTER NOTEBOOK**

<table>
  <tr>
    <td># general start jupyter notebook# in backgroundjupyter notebook &> /dev/null &# check opened notebookjupyter notebook list# close jupyter notebookkill $(pgrep jupyter)</td>
  </tr>
</table>


### **ADD EXTENSIONS TO JUPYTER NOTEBOOK**

Jupyter [nbextensions](https://github.com/ipython-contrib/jupyter_contrib_nbextensions/)

<table>
  <tr>
    <td># install the nbextensionspip install jupyter_contrib_nbextensions# install jsjupyter contrib nbextension install --user# enable nbextensionsjupyter nbextension enable codefolding/main</td>
  </tr>
</table>


### **_NBCONVERT_**** FOR CONVERTING "IPYTHON" TO “PDF”**

<table>
  <tr>
    <td>sudo apt-get install pandocgit clone https://github.com/jupyter/nbconvert.gitcd nbconvertpip install -e .# test nbconvertpip install nbconvert[test]py.test --pyargs nbconvert</td>
  </tr>
</table>


