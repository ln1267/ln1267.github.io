---
layout: post
title:  Anaconda and Jupyter notebook
date: 2018-3-30
excerpt: This is a guild for installing Ananconda and Jupyter notebook
---

## Install and Config Anaconda

### Install Anaconda
-   Anaconda can be downloaded in [Anaconda's website](https://www.continuum.io/downloads#_unix), it has both [Linux](http://repo.continuum.io/archive/Anaconda3-4.0.0-Linux-x86_64.sh) and [Windows version](http://repo.continuum.io/archive/Anaconda3-4.0.0-Windows-x86_64.exe)
-   install Anaconda
    ``` 
    bash *.sh
    ```
-   set the default python3
    After install, restart the connection the default ***".bashrc"*** will be changed for Anaconda
    ```
    # added by Anaconda3 4.0.0 installer
    export PATH="/home/ubuntu/anaconda3/bin:$PATH"

    ```
-   install modules
    ```
    conda list # check installed modules
    conda install netCDF4
    ```
-  Update  
    ```conda update anaconda```
    
- create an environment  
    ```conda create -n py35 python=3.5 anaconda```
    
-   To activate I use  
   ``` source activate py35```
    
-   to deactivate  
    ```source deactivate```  

## Jupyter notebook
### start Jupyter notebook
```
# general start 
jupyter notebook

# in background
jupyter notebook &> /dev/null &

# check opened notebook
jupyter notebook list

# Close jupyter note book
kill $(pgrep jupyter)
```
### add extension to Jupyter
Jupyter nbextensions [link](https://github.com/ipython-contrib/jupyter_contrib_nbextensions/)
```shell
# install the nbextensions
pip install jupyter_contrib_nbextensions
# install js
jupyter contrib nbextension install --user
# enable nbextensions
jupyter nbextension enable codefolding/main

```

### nbconvert for converting ipython to pdf
```
sudo apt-get install pandoc
git clone https://github.com/jupyter/nbconvert.git
cd nbconvert
pip install -e .
# test nbconvert
pip install nbconvert[test]
py.test --pyargs nbconvert
```


