# I2B2MetadataTreeInShiny

Simple port of the I2B2 tree in shiny using shinyTree

It is meant as a simple app to explore the concept tree of I2B2 to gather either code or fullpath to perform query outside of I2B2

It allows for a simple **top down exploration**, each node's children is loaded and appended to the tree on click. Depending on the type (node/leaf/modifier/...) it has a different icon. When a node has children loaded, it has a different icon too

When a node is selected, a sidebar show the C_NAME, C_BASECODE, C_FULLNAME with a button to copy it to the clipboard aswell as other path that share the same code. These other path can be viewed as a tree when opening the "Matching codes" tab. 
This tab has a button "OPEN" to expand all the node in one click (It triggers a javascript message)

The search tab can do : **dynamic regex search on the colum you want**. Results are shown as a explorable tree like in the first tab, except all the results are there from the start
By default searches are triggered by the click of a button.  
Dynamic search means it reevaluates the search each time a character is modified.
You can use regex : (example : Hyp(o|er)gly)
An other "OPEN the tree" is there too  


![Demo](https://user-images.githubusercontent.com/81096103/111912512-41339c80-8a6a-11eb-870c-dcf9a7396ce6.gif)



*Particularly proud of the [insertion of the children inside the tree without modifying the status of each other nodes](https://github.com/Hippolyte-Menou/I2B2MetadataTreeInShiny/blob/main/server.R#L154)*
