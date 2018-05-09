# CS152 Elm

A polling application written in programming language Elm. This application will not be using the latest version of ELM 0.18.0, but ELM 0.16.0 (the FireBase open source project we used was only available in 0.16). In addition, this application will be using the Firebase API - ElmFire.

The following links show the changes from ELM 0.18.0 to 0.17.0 and from 0.17.0 to 0.16.0
- ELM 0.18.0 -> ELM 0.17.0 : https://github.com/elm-lang/elm-platform/blob/master/upgrade-docs/0.18.md
- ELM 0.17.0 -> ELM 0.16.0 : https://github.com/elm-lang/elm-platform/blob/master/upgrade-docs/0.17.md

## Install

This guide will give you instructions on how to install Elm :  
- https://guide.elm-lang.org/install.html    

Please also ensure that you have NPM installed :
- https://www.npmjs.com/get-npm

In order to use ELM Environment 0.16.0, please install Elm version manager :   
- https://www.npmjs.com/package/elm-version-manager

It is worth noting that some of the commands may need to be run from root, in which case just add 'sudo' at the far left.

```
$ npm install -g elm-version-manager
$ evm install 0.16.0
$ evm use 0.16.0    
```
## Elm to HTML

To compile Elm into an HTML: 

1) Run this in terminal.
```
$ elm-make (Name of File).elm --output=(Name of Application).html
```
2) If it asks you to approve of downloading files, type "y" and hit enter
  -This will create a folder called elm-stuff
3) Repeat from step 1 for all .elm files

If there are ever any complications, or if the command line says 'Success! Compiled 0 modules.', then try deleting 'elm-stuff' and recompile.

We will compile by running in terminal :
```
$ elm-make questions.elm --output=questions.html
$ elm-make answers.elm --output=answers.html
$ elm-make results.elm --output=results.html
```
