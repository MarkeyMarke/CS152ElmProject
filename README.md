# CS152 Elm

A polling application written in programming language Elm. This application will not be using the latest version of ELM 0.18.0, but ELM 0.16.0. In addition, this application will be using the Firebase API - ElmFire.

The following links show the changes from ELM 0.18.0 to 0.17.0 and from 0.17.0 to 0.16.0
- ELM 0.18.0 -> ELM 0.17.0 : https://github.com/elm-lang/elm-platform/blob/master/upgrade-docs/0.18.md
- ELM 0.17.0 -> ELM 0.16.0 : https://github.com/elm-lang/elm-platform/blob/master/upgrade-docs/0.17.md

## Install

This guide will give you instructions on how to install Elm :  
- https://guide.elm-lang.org/install.html    

In order to use ELM Environment 0.16.0, please install ELM version manager :   
- https://www.npmjs.com/package/elm-version-manager  

```
$ evm use 0.16.0    
```
## Elm to HTML

To compile Elm into an HTML: 

1) If you have a folder called elm-stuff, delete it.
2) Run this in terminal.
```
$ elm-make (Name of File).elm --output=(Name of Application).html
```
3) When it asks you to approve of downloading files, type "y" and hit enter
4) Repeat from step 1 for all .elm files

We will compile by running in terminal :
```
$ elm-make questions.elm --output=questions.html
$ elm-make answers.elm --output=answers.html
$ elm-make results.elm --output=results.html
```
