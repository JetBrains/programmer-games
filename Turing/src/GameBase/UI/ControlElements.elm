module GameBase.UI.ControlElements exposing (runButtonDraw, runFastDraw, 
                                             quesButtonDraw, helpMsgDraw, 
                                             levelDraw)
import Svg exposing (Svg, image, text, text')
import Svg.Attributes exposing ( fontSize, fontStyle, width, height, x, y,      
                                 xlinkHref)  
                                 

runButtonDraw : List (Svg msg)                                                  
runButtonDraw =                                                                 
  [ image                                                                   
      [ x "523px"                                                             
      , y "315px"                                                             
      , width "70px"                                           
      , height "70px"                                          
      , xlinkHref ("../img/elements/run.png")                                 
      ]                                                                       
      []                                                                      
  ]                                                                             
                                                                                
                                                                                
runFastDraw : List (Svg msg)                                                    
runFastDraw =                                                                   
  [ image                                                                   
      [ x "449px"                                                             
      , y "315px"                                                             
      , width "70px"                                           
      , height "70px"                                          
      , xlinkHref ("../img/elements/runFast.png")                             
      ]                                                                       
      []                                                                      
  ]                                                                             
                                                                                
                                                                                
quesButtonDraw : List (Svg msg)                                                 
quesButtonDraw =                                                                
  [ image                                                                   
      [ x "624px"                                                             
      , y "322px"                                                             
      , width "30px"                                           
      , height "55px"                                          
      , xlinkHref ("../img/elements/ques.png")                                
      ]                                                                       
      []                                                                      
  ]                                                                             
                                                                                
                                                                                
helpMsgDraw : String -> List (Svg msg)                                          
helpMsgDraw hmsg =                                                              
  [ image                                                                  
      [ x "10px"                                                              
      , y "10px"                                                              
      , width "537px"                                          
      , height "40px"                                          
      , xlinkHref (hmsg)                                                      
      ]                                                                       
      []                                                                      
  ]  


levelDraw : Int -> Int -> List (Svg msg)                                        
levelDraw level max =                                                           
  [ text'
      [ x "695px"                                                             
      , y "360px"                                                             
      , fontStyle "italic"                                                    
      , fontSize "30px"                                                       
      ]                                                                       
      [ text ((toString level) ++ "/" ++ (toString max)) ]                
  ] 
