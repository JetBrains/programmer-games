The project Learn Elm #1:

2 buttons with + and -, changing the number N in between 3 and 12.
On mouse click on screen a N-polygon is drawn with the center in click and random radius between 20 and 100.
There are three squares user can click on to select one of predefined colors.

The project Turing:

Implementation of Turing machine. Use function "runMachine" in Main.elm, give it machine and input word. It will print tape and final state for you. 

Run the following commands in your terminal to download this projects and start a server that compiles them for you:

Run the project 1:

    git clone https://github.com/JetBrains/programmer-games.git 

    cd programmer-games/LearnElm_1

    elm-reactor

Now go to http://localhost:8000/, click src and then choose Main.elm file.

Run the project 2:

    git clone https://github.com/JetBrains/programmer-games.git

    cd programmer-games/Turing    

    elm-reactor 

Now go to http://localhost:8000/, click src and then choose Main.elm file.

For running tests from Turing/test:

    git clone https://github.com/JetBrains/programmer-games.git

    cd programmer-games/Turing

    elm-make test/TestRunner.elm --output test.js

    node test.js

Then you will see the results of the tests running.
