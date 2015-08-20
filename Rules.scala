/**
 * Created by jaighanekar on 6/30/15.
 */

import java.util.NoSuchElementException
import scala.util.Sorting
import scala.collection.mutable.Stack



object Rules {

    //applies to every player still playing

    def findWinner(list:Card*) {

      highCard(list(0),list(1))
    }


    // def noHands the player got nothing bad luck

    def highCard(cards:Card*): String = {
      //scala pattern matching
      val rank = new Array[Int](7)
      var count:Int = 0

      for(values <- cards) {
        val value: Int = values.cardNumber match {
          case "Ace" => 14
          case "King" => 13
          case "Queen" => 12
          case "Jack" => 11
          case "10" => 10
          case "9" => 9
          case "8" => 8
          case "7" => 7
          case "6" => 6
          case "5" => 5
          case "4" => 4
          case "3" => 3
          case "2" => 2
        //give the values to each card
        }
        rank(count) = value
        count+=1

      }
      Sorting.quickSort(rank)
      val name = Map(
        14 -> "Ace",
        13 -> "King",
        12 -> "Queen",
        11 -> "Jack",
        10 -> "10", 9 -> "9" , 8 -> "8" , 7 -> "7" , 6 -> "6" , 5 -> "5" , 4 -> "4" , 3 -> "3" , 2 -> "2"
      )

      "High Card " + name(rank.last) // convert the value to its name
    }

    //define a pair

    def pair(cards:Card*): String = {
      //define a method to count the frequency of each card


      // map cards to their count



      def cardcount(cardnums:String*) : String = {
        val temp:Array[Int] = new Array[Int](13) //count the cards
        var i = 0
        for (i <- 0 to temp.length - 1 ){temp(i) = 0}

        for (items <- cardnums){
          items match {
            case "Ace" => temp(0) += 1
            case "King" => temp(1) += 1
            case "Queen" => temp(2) += 1
            case "Jack" => temp(3) += 1
            case "10" => temp(4) += 1
            case "9" => temp(5)+=1
            case "8" => temp(6) += 1
            case "7" => temp(7) += 1
            case "6" => temp(8)+= 1
            case "5" => temp(9) += 1
            case "4" => temp(10) += 1
            case "3" => temp(11) += 1
            case "2" => temp(12) += 1
          }
        }
        val count:Map[String, Int] = Map(
          "Ace" -> temp(0), "King" -> temp(1) ,

          "Queen" -> temp(2) , "Jack" -> temp(3) ,

          "10" -> temp(4) , "9" -> temp(5) ,

          "8" -> temp(6) , "7" -> temp(7) , "6" -> temp(8) , "5" -> temp(9) , "4" -> temp(10) ,

          "3" -> temp(11) , "2" -> temp(12)



        )
        // reverse the hashmap and return the string with a pair
        val reverseMap = for ((k,v) <- count) yield (v, k)
        try{
          "Pair of " + reverseMap(2) + "s"
        }
        catch{
          case e : NoSuchElementException => highCard(cards(0),cards(1)).toString //if two doesn't exist
        }
      }
      val current_string:String = cardcount(cards(0).number,cards(1).number,cards(2).number,cards(3).number,
      cards(4).number, cards(5).number, cards(6).number)
      current_string

    }

    def twoPair(cards:Card*) : String = {
      // call the pair method then try to see if there is another pair


      def cardcount(cardnums:String*) : String = {
        val temp:Array[Int] = new Array[Int](13) //count the cards
        var i:Int = 0
        for (i <- 0 to temp.length - 1 ){temp(i) = 0}

        for (items <- cardnums){
          items match {
            case "Ace" => temp(0) += 1
            case "King" => temp(1) += 1
            case "Queen" => temp(2) += 1
            case "Jack" => temp(3) += 1
            case "10" => temp(4) += 1
            case "9" => temp(5)+=1
            case "8" => temp(6) += 1
            case "7" => temp(7) += 1
            case "6" => temp(8)+= 1
            case "5" => temp(9) += 1
            case "4" => temp(10) += 1
            case "3" => temp(11) += 1
            case "2" => temp(12) += 1
          }
        }
        //map the keys to a value
        val count:Map[String, Int] = Map(
          "Ace" -> temp(0), "King" -> temp(1) ,

          "Queen" -> temp(2) , "Jack" -> temp(3) ,

          "10" -> temp(4) , "9" -> temp(5) ,

          "8" -> temp(6) , "7" -> temp(7) , "6" -> temp(8) , "5" -> temp(9) , "4" -> temp(10) ,

          "3" -> temp(11) , "2" -> temp(12)



        )
        // search keys in the hashmap and return the string with a pair of 2
        val stringbox:Array[String] = Array(
          "Ace","King","Queen", "Jack" , "10" ,"9" , "8" , "7" , "6" ,
          "5", "4" , "3", "2") // this will store the keys in the loop

        val pair_catcher:Array[String] = new Array[String](2) // this will store the pair values

        //loop through the strings to find a value matching two
        var placevalue = 0 //this will be used to fill the array
        for(key <- stringbox){
          //if the key is matched up with a two then add that to array

          if(count(key) == 2 ){
            pair_catcher(placevalue) = key
            placevalue += 1
            //after it gets to two
            if(placevalue == 2 ){
              return "Pair of " + pair_catcher(0) +"s " + "and " + pair_catcher(1) +"s"
            }
          }
          if(placevalue > 2 ){
            return "Pair of " + pair_catcher(0) +"s " + "and " + pair_catcher(1) +"s"
          }
        }

        if(placevalue == 1){
          pair(cards(0),cards(1),cards(2),cards(3),cards(4),cards(5),cards(6)) //if there is one pair
        }
        else{
         highCard(cards(0),cards(1)) //there are no found pairs
        }

      }
      //use the embedded method
      val current_string:String = cardcount(cards(0).number,cards(1).number,cards(2).number,cards(3).number,
        cards(4).number, cards(5).number, cards(6).number)
      current_string


    }

    def trips(cards:Card*): String = {
      // call the pair method then try to see if there are triples


      def cardcount(cardnums: String*): String = {
        val temp: Array[Int] = new Array[Int](13) //count the cards
        var i: Int = 0
        for (i <- 0 to temp.length - 1) {
          temp(i) = 0
        }

        for (items <- cardnums) {
          items match {
            case "Ace" => temp(0) += 1
            case "King" => temp(1) += 1
            case "Queen" => temp(2) += 1
            case "Jack" => temp(3) += 1
            case "10" => temp(4) += 1
            case "9" => temp(5) += 1
            case "8" => temp(6) += 1
            case "7" => temp(7) += 1
            case "6" => temp(8) += 1
            case "5" => temp(9) += 1
            case "4" => temp(10) += 1
            case "3" => temp(11) += 1
            case "2" => temp(12) += 1
          }
        }
        //map the keys to a value
        val count: Map[String, Int] = Map(
          "Ace" -> temp(0), "King" -> temp(1),

          "Queen" -> temp(2), "Jack" -> temp(3),

          "10" -> temp(4), "9" -> temp(5),

          "8" -> temp(6), "7" -> temp(7), "6" -> temp(8), "5" -> temp(9), "4" -> temp(10),

          "3" -> temp(11), "2" -> temp(12)


        )
        // search keys in the hashmap and return the string with a pair of 2
        val stringbox: Array[String] = Array(
          "Ace", "King", "Queen", "Jack", "10", "9", "8", "7", "6",
          "5", "4", "3", "2") // this will store the keys in the loop


        //loop through the strings to find a value matching two
        var placevalue = 0 //this will be used to fill the array
        for (key <- stringbox) {
          //if the key is matched up with a three just print to display

          if (count(key) == 3) {
           return "Triple " + key + "s"
          }

        }


        twoPair(cards(0), cards(1), cards(2), cards(3), cards(4), cards(5), cards(6)) //there are no found trips


      }

      //use the embedded method
      val current_string:String = cardcount(cards(0).number,cards(1).number,cards(2).number,cards(3).number,
        cards(4).number, cards(5).number, cards(6).number)
      current_string
    }

    def straight(cards:Card*): String = {

      def cardcount(cardnums: String*): String = {
        val temp: Array[Int] = new Array[Int](13) //count the cards
        var i: Int = 0
        for (i <- 0 to temp.length - 1) {
          temp(i) = 0
        }

        for (items <- cardnums) {
          items match {
            case "Ace" => temp(0) += 1
            case "King" => temp(1) += 1
            case "Queen" => temp(2) += 1
            case "Jack" => temp(3) += 1
            case "10" => temp(4) += 1
            case "9" => temp(5) += 1
            case "8" => temp(6) += 1
            case "7" => temp(7) += 1
            case "6" => temp(8) += 1
            case "5" => temp(9) += 1
            case "4" => temp(10) += 1
            case "3" => temp(11) += 1
            case "2" => temp(12) += 1
          }
        }
        //map the keys to a value
        val count: Map[String, Int] = Map(
          "Ace" -> temp(0), "King" -> temp(1),

          "Queen" -> temp(2), "Jack" -> temp(3),

          "10" -> temp(4), "9" -> temp(5),

          "8" -> temp(6), "7" -> temp(7), "6" -> temp(8), "5" -> temp(9), "4" -> temp(10),

          "3" -> temp(11), "2" -> temp(12)


        )
        // create boolean values for each possible straight and if there is a value to match print type
        val A_5:Boolean = count("Ace") >= 1 && count("2") >= 1 && count("3") >= 1 && count("4") >= 1 && count("5") >= 1
        val two_6:Boolean = count("2") >= 1 && count("3") >= 1 && count("4") >= 1 && count("5") >= 1 && count("6") >= 1
        val three_7:Boolean = count("3")>= 1 && count("4") >= 1 && count("5") >= 1 && count("6") >= 1 && count("7") >= 1
        val four_8:Boolean = count("4") >= 1 && count("5") >= 1 && count("6") >= 1 && count("7") >= 1 && count("8") >= 1
        val five_9:Boolean = count("5") >= 1 && count("6") >= 1 && count("7") >= 1 && count("8") >= 1 && count("9") >= 1
        val six_10:Boolean = count("6") >= 1 && count("7") >= 1 && count("8") >= 1 && count("9") >= 1 && count("10") >= 1
        val seven_jack:Boolean = count("7") >= 1 && count("8") >= 1 && count("9") >= 1 && count("10") >= 1 && count("Jack")>= 1
        val eight_queen:Boolean = count("8") >= 1 && count("9") >= 1 && count("10") >= 1 && count("Jack") >= 1 && count("Queen")>= 1
        val nine_king:Boolean = count("9") >= 1 && count("10") >= 1 && count("Jack") >= 1 && count("Queen") >= 1 && count("King")>= 1
        val ten_ace:Boolean = count("10") >= 1 && count("Jack") >= 1 && count("Queen") >= 1 && count("King") >= 1 && count("Ace")>= 1

        //map the name of each string to a boolean value and return straight or test triple

        val straightfinder = Map(
        "Straight:Ace-Five" -> A_5 , "Straight:Two-Six" -> two_6 , "Straight:Three-Seven" -> three_7,
        "Straight:Four-Eight" -> four_8 , "Straight:Five-Nine" -> five_9 , "Straight:Six-Ten" -> six_10,
        "Straight:Seven-Jack" -> seven_jack, "Straight:Eight-Queen" -> eight_queen , "Straight:Nine-King" -> nine_king,
        "Straight:Ten-Ace" -> ten_ace)

        //store all the keys in a string array

        val cardholder:Array[String] = Array(
          "Straight:Ace-Five","Straight:Two-Six","Straight:Three-Seven", "Straight:Four-Eight",
          "Straight:Five-Nine", "Straight:Six-Ten", "Straight:Seven-Jack", "Straight:Eight-Queen",
           "Straight:Nine-King", "Straight:Ten-Ace")
        //iter through array for true value

        for (straights <- cardholder){
          if (straightfinder(straights)){
            return straights
          }
        }


        trips(cards(0), cards(1), cards(2), cards(3), cards(4), cards(5), cards(6)) //there are no found trips

      }







      //use the embedded method
      val current_string:String = cardcount(cards(0).number,cards(1).number,cards(2).number,cards(3).number,
        cards(4).number, cards(5).number, cards(6).number)
      current_string
    }

    def flush(cards:Card*): String = {
      def cardcount(cardsuit: String*): String = {
        val temp: Array[Int] = new Array[Int](4) //count the cards
        var i: Int = 0
        for (i <- 0 to temp.length - 1) {
          temp(i) = 0
        }

        for (items <- cardsuit) {
          items match {
            case "Club" => temp(0) += 1
            case "Diamond" => temp(1) += 1
            case "Heart" => temp(2) += 1
            case "Spade" => temp(3) += 1
          }
        }
        //map the keys to a value
        val count: Map[String, Int] = Map(
          "Club" -> temp(0), "Diamond" -> temp(1),

          "Heart" -> temp(2), "Spade" -> temp(3)

        )
        // search keys in the hashmap and return the string with a pair of 2
        val stringbox: Array[String] = Array(
          "Club", "Diamond", "Heart", "Spade") // this will store the keys in the loop


        //loop through the strings to find a value matching two
        for (key <- stringbox) {
          //if the key is matched up with a three just print to display

          if (count(key) == 5) {
            return "Flush: " + key + "s"
          }

        }


        straight(cards(0), cards(1), cards(2), cards(3), cards(4), cards(5), cards(6)) //there are no found trips


      }

      //use the embedded method
      val current_string:String = cardcount(cards(0).suit,cards(1).suit,cards(2).suit,cards(3).suit,
        cards(4).suit, cards(5).suit, cards(6).suit)
      current_string
    }

    def fullhouse(cards:Card*): String = {
      // call the pair method then try to see if there is another pair


      def cardcount(cardnums: String*): String = {
        val temp: Array[Int] = new Array[Int](13) //count the cards
        var i: Int = 0
        for (i <- 0 to temp.length - 1) {
          temp(i) = 0
        }

        for (items <- cardnums) {
          items match {
            case "Ace" => temp(0) += 1
            case "King" => temp(1) += 1
            case "Queen" => temp(2) += 1
            case "Jack" => temp(3) += 1
            case "10" => temp(4) += 1
            case "9" => temp(5) += 1
            case "8" => temp(6) += 1
            case "7" => temp(7) += 1
            case "6" => temp(8) += 1
            case "5" => temp(9) += 1
            case "4" => temp(10) += 1
            case "3" => temp(11) += 1
            case "2" => temp(12) += 1
          }
        }
        //map the keys to a value
        val count: Map[String, Int] = Map(
          "Ace" -> temp(0), "King" -> temp(1),

          "Queen" -> temp(2), "Jack" -> temp(3),

          "10" -> temp(4), "9" -> temp(5),

          "8" -> temp(6), "7" -> temp(7), "6" -> temp(8), "5" -> temp(9), "4" -> temp(10),

          "3" -> temp(11), "2" -> temp(12)


        )
        // search keys in the hashmap and return the string with a pair of 2
        val stringbox: Array[String] = Array(
          "Ace", "King", "Queen", "Jack", "10", "9", "8", "7", "6",
          "5", "4", "3", "2") // this will store the keys in the loop


        //loop through the strings to find a value matching two
        var placevalue = 0 //this will be used to fill the array
        for (key <- stringbox) {
          //if the key is matched up with a three just print to display

          if (count(key) == 3) {
            for (items <- stringbox){
              if (count(items) == 2){
                return "Full House" //this will be the if a pair is count
              }
            }
          }

        }


        flush(cards(0), cards(1), cards(2), cards(3), cards(4), cards(5), cards(6)) //there are no found trips


      }

      //use the embedded method
      val current_string:String = cardcount(cards(0).number,cards(1).number,cards(2).number,cards(3).number,
        cards(4).number, cards(5).number, cards(6).number)
      current_string
    }

    def fours(cards:Card*): String = {
      // call the pair method then try to see if there are triples


      def cardcount(cardnums: String*): String = {
        val temp: Array[Int] = new Array[Int](13) //count the cards
        var i: Int = 0
        for (i <- 0 to temp.length - 1) {
          temp(i) = 0
        }

        for (items <- cardnums) {
          items match {
            case "Ace" => temp(0) += 1
            case "King" => temp(1) += 1
            case "Queen" => temp(2) += 1
            case "Jack" => temp(3) += 1
            case "10" => temp(4) += 1
            case "9" => temp(5) += 1
            case "8" => temp(6) += 1
            case "7" => temp(7) += 1
            case "6" => temp(8) += 1
            case "5" => temp(9) += 1
            case "4" => temp(10) += 1
            case "3" => temp(11) += 1
            case "2" => temp(12) += 1
          }
        }
        //map the keys to a value
        val count: Map[String, Int] = Map(
          "Ace" -> temp(0), "King" -> temp(1),

          "Queen" -> temp(2), "Jack" -> temp(3),

          "10" -> temp(4), "9" -> temp(5),

          "8" -> temp(6), "7" -> temp(7), "6" -> temp(8), "5" -> temp(9), "4" -> temp(10),

          "3" -> temp(11), "2" -> temp(12)


        )
        // search keys in the hashmap and return the string with a pair of 2
        val stringbox: Array[String] = Array(
          "Ace", "King", "Queen", "Jack", "10", "9", "8", "7", "6",
          "5", "4", "3", "2") // this will store the keys in the loop


        //loop through the strings to find a value matching two
        var placevalue = 0 //this will be used to fill the array
        for (key <- stringbox) {
          //if the key is matched up with a three just print to display

          if (count(key) == 4) {
            return "Four of a kind " + key + "s"
          }

        }


        fullhouse(cards(0), cards(1), cards(2), cards(3), cards(4), cards(5), cards(6)) //there are no found trips


      }

      //use the embedded method
      val current_string:String = cardcount(cards(0).number,cards(1).number,cards(2).number,cards(3).number,
        cards(4).number, cards(5).number, cards(6).number)
      current_string
    }

    def straightflush(cards:Card*) : String = {
      if (straight(cards(0), cards(1), cards(2), cards(3) , cards(4), cards(5), cards(6)).contains("Straight")
      && flush(cards(0), cards(1), cards(2), cards(3) , cards(4), cards(5), cards(6)).contains("Flush")){
        return "Straight Flush" //if the cards have a straight and a flush
      }
      else{
        //pass to fours
        fours(cards(0), cards(1), cards(2), cards(3) , cards(4), cards(5), cards(6))
      }
    }



  }
