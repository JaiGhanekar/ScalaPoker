import java.text.NumberFormat
import javax.swing.{SwingConstants, ImageIcon, JPanel}
import scala.util.Sorting
import swing._
import scala.math.Ordered._

import scala.swing.{SimpleSwingApplication, SwingApplication}
import event._
import scala.swing.BorderPanel.Position._



/**
 * Created by jaighanekar on 6/13/15.
 */

object MainApp {
  def main(args:Array[String]) = {
    App.main(args)
  }

}


object App extends SimpleSwingApplication {

  private val deckObj = new Deck() //create a new deck to play with
  deckObj.shuffle()
  private val card1 = deckObj.nextCard()
  private val card2 = deckObj.nextCard() //cards in players hand
  //declare table objects

  private val burn1 = deckObj.nextCard()
  private val tcard = deckObj.nextCard() //table cards and burn cards
  private val tcard2 = deckObj.nextCard()
  private val tcard3 = deckObj.nextCard()
  private val burn2 = deckObj.nextCard()
  private val tcard4 = deckObj.nextCard()
  private val burn3 = deckObj.nextCard()
  private val tcard5 = deckObj.nextCard()

  def top = new MainFrame {
    title = "Scala Poker"
    size = new Dimension(3000,3000) //set the dimensions of the GUI
   //show dialog to initialize inputs
    var holding_amount:Float = 0.0f
    var pot_amount:Float = 0.0f

    //tells you how much money you have
    val holding = new Label{
      text = "Holding: $" + holding_amount + "   "
    }

    //tells you how much is at stake
    val pot = new Label{
      text = "Pot:       $" + pot_amount + "   "
    }

   //displaying the dialog to intialize the pot





    val box = new Dialog{
      title = "Initalizer"
      //lables and textFields
     resizable = false
     val inner_holding = new Label{
       text = "Holding: $"
     }
     //text for the dialog
     val holdingText = new FormattedTextField(NumberFormat.getNumberInstance()){} //format this for bad input


     val submit = new Button{
       action = Action("Submit"){
         holding_amount = holdingText.text.toFloat //get the edited text
         holding.text = "Holding: $" + holding_amount + "   "
         if(holding_amount!=0.0f) {
           holdingText.editable = false

         }

       }
     }




      //add to grid then add to box
      val innerGrid = new GridPanel(1,3){
        contents += inner_holding
        contents += holdingText
        contents += submit
      }
      contents = innerGrid

    }

    val settings = new Button{
      action = Action("Settings"){
        box.open()
      }
    }


    //open box
    val westGrid = new GridPanel(3,1){
      contents += holding
      contents += pot
      contents += settings
    }






  //labels for all of the cards

    val handcard1 = new Label{
      icon = new ImageIcon(card1.image)  //give the image a path
    }

    val handcard2 = new Label{
      icon = new ImageIcon(card2.image)

    }

    //declare center objects
    val burnpile = new Label{
      icon = new ImageIcon(burn1.back)
      visible = false
    }

    val tcardlabel1 = new Label{
      icon = new ImageIcon(tcard.image)
      visible = false

    }

    val tcardlabel2 = new Label{
      icon = new ImageIcon(tcard2.image)
      visible = false

    }

    val tcardlabel3 = new Label{
      icon = new ImageIcon(tcard3.image)
      visible = false

    }

    //set these image icons after the round ends
    val tcardlabel4 = new Label{
      icon = new ImageIcon(tcard4.image)
      visible = false

    }

    val tcardlabel5 = new Label{
      icon = new ImageIcon(tcard5.image)
      visible = false

    }



    val grid = new GridPanel(1,2){

      contents += handcard1
      contents += handcard2

    }

    val grid2 = new GridPanel(1,6){
      contents += burnpile
      contents += tcardlabel1
      contents += tcardlabel2
      contents += tcardlabel3
      contents += tcardlabel4
      contents+= tcardlabel5
    }
    var turn:Int = 0
    var raiseAmount:Float = 0.0f
    var display:String = ""
    val message = new Label{

      visible = false

      display = Rules.straightflush(card1,card2, tcard, tcard2 , tcard3 , tcard4 , tcard5) //evaluate

      text = display

    }

    val Check = new Button("Check"){
      // if player has enough money and all players check or there is no bet


      //if there is money in holding set visible to true

      action = Action("Check"){
        turn += 1
        if(turn > 0){
          settings.visible = false
          burnpile.visible = true
          tcardlabel1.visible = true
          tcardlabel2.visible = true
          tcardlabel3.visible = true

        }
        if(turn > 1 ){
          tcardlabel4.visible = true
        }
        if(turn > 2){
          tcardlabel5.visible = true
          turn +=1
        }
        if (turn > 3){
          message.visible = true
        }

      }
    }





    //if fold == true get rid of the buttons and player cards
    var didFold = false
    val raisetext = new FormattedTextField(NumberFormat.getNumberInstance()) {} //add code for input validation


    val Raise = new Button("Raise"){


      action = Action("Bet/Raise"){
        raiseAmount = raisetext.text.toFloat
        holding_amount-= raiseAmount //subtract from holding amount
        holding.text = "Holding: $" + holding_amount + "   " //update the label
        pot_amount += raiseAmount
        pot.text = "Pot:       $" + pot_amount + "   "

        turn += 1
        if(turn > 0){
          settings.visible = false
          burnpile.visible = true
          tcardlabel1.visible = true
          tcardlabel2.visible = true
          tcardlabel3.visible = true
        }
        if(turn > 1 ){
          tcardlabel4.visible = true
        }
        if(turn > 2){
          tcardlabel5.visible = true
          turn +=1
        }
        if (turn > 3){
          message.visible = true

        }



      }
    }

    //fold method set is playing to false
    val Fold = new Button("Fold"){
      action = Action("Fold"){
        didFold = true
        handcard1.visible = false
        handcard2.visible = false
        visible = false
        Check.visible = false
        Call.visible = false
        raisetext.visible =false
        Raise.visible = false
        //System.exit(1)
      }
    }

    val Call = new Button {


      action = Action("Call") {

        if (holding_amount >= raiseAmount) {
          holding_amount -= raiseAmount //subtract from holding amount
          holding.text = "Holding: $" + holding_amount + "   " //update the label
          pot_amount += raiseAmount
          pot.text = "Pot:       $" + pot_amount + "   "
        }




        turn += 1
        if (turn > 0) {
          settings.visible = false
          burnpile.visible = true
          tcardlabel1.visible = true
          tcardlabel2.visible = true
          tcardlabel3.visible = true
        }
        if (turn > 1) {
          tcardlabel4.visible = true
        }
        if (turn > 2) {
          tcardlabel5.visible = true
          turn += 1
        }
        if (turn > 3) {
          message.visible = true

        }
      }
    }





    val textPanel = new GridPanel(5,1){

      contents += Check
      contents += Call
      contents += raisetext
      contents += Raise
      contents += Fold  //add buttons to grid

    }








  //the main layout
    val border = new BorderPanel{
      layout(grid) = South
      layout(grid2) = Center
      layout(textPanel) = East
      layout(westGrid) = West
      layout(message) = North
    }




    //set resizeable false

    resizable = false


    contents = border

    //listen to the cards and flip them on click

    listenTo(handcard1.mouse.clicks)
    listenTo(handcard2.mouse.clicks)


    //react to the event
    var clicks1:Int = 0

    reactions += {
      case e: MouseClicked =>
        clicks1+=1
        if (clicks1 %2 != 0) {
          handcard1.icon = new ImageIcon(card1.back)
          handcard2.icon = new ImageIcon(card2.back)
        }
        else {
          handcard1.icon = new ImageIcon(card1.image)       //used to flip the image to hide cards
          handcard2.icon = new ImageIcon(card2.image)
        }



    }

    //setting game
    if(holding_amount <= 0.0f){
      textPanel.visible = false //dont give the player options if they don't have money
    }
    if(holding_amount >= 0.0f){
      textPanel.visible = true
    }







    pack()

    //define a method for hiding settings button






  }










}


