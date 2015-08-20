import java.awt.image.{ImageObserver, BufferedImage}
import javax.imageio.ImageIO
import java.io.File
import java.awt.Graphics2D
import java.awt.Graphics




/**
 * Created by jaighanekar on 6/5/15.
 */
object Game  {

  //def main(args: Array[String]){
  def start():Unit = {
    val deck = new Deck()
    deck.shuffle() //shuffle the new deck
    val playerArray:Array[Player] = initialize(Array("Jai", "Yash", "Mom","Dad")) // pass the name of players


    //distribute the cards one by one poker style make this into a method in main later
    distribute(playerArray, deck)

    // a method to deal the cards on table


    // display hand

    displayHand(playerArray)




    }

    //creates players
    def initialize(name:Array[String]) : Array[Player] = {
      //variable number of players
     val playerArray = new Array[Player](name.length) //array of three players
     //initialization loop
     var count:Int = 0
     for(players<-name){
       playerArray(count) = new Player(players)
       count+=1
     }
       playerArray
    }

    //give the players cards
    def distribute(playerArray:Array[Player], DeckObj:Deck) : Unit = {
      for(players<- playerArray){
        players.firstcard = DeckObj.nextCard()
      }
      for(players<- playerArray){
        players.secondcard = DeckObj.nextCard()
      }
    }

    //display hands
    def displayHand(playerArray:Array[Player]) : Unit = {
      for(players <- playerArray){
        println(players.playerName + " " + players.firstcard.face + " and " + players.secondcard.face)
      }
    }


  }




