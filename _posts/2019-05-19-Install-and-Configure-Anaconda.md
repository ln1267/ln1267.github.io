---
title: Install and Configure Anaconda
layout: post
author: ln1267
permalink: /install-and-configure-anaconda/
source-id: 1oiSxq7BypB4mIdJpj6SPxmP4pMntMnJZ6WTUR3AR5yg
published: true
---
# Install and Configure Anaconda

## Install Anaconda

-   Anaconda can be downloaded from [Anaconda's website](https://www.continuum.io/downloads#_unix). It has both [Linux](http://repo.continuum.io/archive/Anaconda3-4.0.0-Linux-x86_64.sh) and [Windows versions](http://repo.continuum.io/archive/Anaconda3-4.0.0-Windows-x86_64.exe)

-   install Anaconda

```{bash} 

    bash *.sh

```

-   After installation, set the default python3 version by adding the following code to ***".bashrc"***. Then restart the connection. The default ***".bashrc"*** will be changed for Anaconda

```{bash}

    # added by Anaconda3 4.0.0 installer

    export PATH="/home/ubuntu/anaconda3/bin:$PATH"

```

-   install modules

```{bash}

    conda list # check installed modules

    conda install netCDF4

```

-  update  

```conda update anaconda```

    

- create an environment  

```conda create -n py35 python=3.5 anaconda```

    

- activate and use the created environment  

``` source activate py35```

    

-  deactivate an environment    

```source deactivate py35```  

# Jupyter Notebook

## start Jupyter Notebook

```{bash}

# general start 

jupyter notebook

# in background

jupyter notebook &> /dev/null &

# check opened notebook

jupyter notebook list

# close jupyter notebook

kill $(pgrep jupyter)

```

## add extensions to Jupyter Notebook

Jupyter [**nbextensions**](https://github.com/ipython-contrib/jupyter_contrib_nbextensions/)

```shell

# install the nbextensions

pip install jupyter_contrib_nbextensions

# install js

jupyter contrib nbextension install --user

# enable nbextensions

jupyter nbextension enable codefolding/main

```

## ***nbconvert*** for converting "ipython" to "pdf"

```

sudo apt-get install pandoc

git clone https://github.com/jupyter/nbconvert.git

cd nbconvert

pip install -e .

# test nbconvert

pip install nbconvert[test]

py.test --pyargs nbconvert

```

![This is the trest image](https://ars.els-cdn.com/content/image/1-s2.0-S0022169416308307-gr1.jpg)

