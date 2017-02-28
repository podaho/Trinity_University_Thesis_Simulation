package Simulation

import collection.mutable.HashMap
import scala.collection.mutable.Buffer

class LocationGrid {
  var range = 20
  var accuracy = 100
  var locations = HashMap[String,(Int,Int)]()
  var feed = Buffer[(String,String,Int,Int,Int,Int)]() //AgentID, Time, Time for Sorting, Accuracy, X, Y
  var results = Buffer[(String,String,String)]() //Agent i, Agent j, Time
  var readPercent = 0.0
  var processPercent = 0.0
  
  def setAgents(lst:Buffer[String]) {
    lst.foreach(x => {
      locations += (x -> (20000000,20000000))
    })
  }
  
  def process() {
    feed.foreach(x => {
      println(x._1+", "+x._2+", "+x._3.toString()+", Accuracy:"+x._4.toString()+", X:"+x._5.toString()+", Y:"+x._6.toString())
    })
    var i = 6010000
    var i2 = 0
    var j = 0
    while(i < 105012400) {
      while(j < feed.size && feed(j)._3 == i) {
        if(feed(j)._4 > accuracy) {
          locations(feed(j)._1) = (20000000,20000000)
        } else {
          locations(feed(j)._1) = (feed(j)._5,feed(j)._6)
        }
        j+=1
      }
      checkInRange(i)
      if((i2)/(482400).toDouble > processPercent) {
        processPercent += 0.01
        println("Processing... " + (processPercent*100).toInt.toString() + "%")
      }
      i = nextMinute(i)
      i2 += 1
    }
  }
  
  def nextMinute(input:Int):Int = {
    var ret = input + 1
    if(ret%100==60) {
      ret -= 60
      ret += 100
    }
    if(ret%10000==2400) {
      ret -= 2400
      ret += 10000
    }
    if(ret%1000000==290000 && ((ret%100000000)/1000000.0).floor==2) {
      ret -= 290000
      ret += 1010000
    } else if(ret%1000000==310000 && (((ret%100000000)/1000000.0).floor==6.0 || ((ret%100000000)/1000000.0).floor==9.0 || ((ret%100000000)/1000000.0).floor==11.0 || ((ret%100000000)/1000000.0).floor==4.0)) {
      ret -= 310000
      ret += 1010000
    } else if(ret%1000000==320000 && (((ret%100000000)/1000000.0).floor==7.0 || ((ret%100000000)/1000000.0).floor==8.0 || ((ret%100000000)/1000000.0).floor==10.0 || ((ret%100000000)/1000000.0).floor==12.0 || ((ret%100000000)/1000000.0).floor==1.0 || ((ret%100000000)/1000000.0).floor==3.0 || ((ret%100000000)/1000000.0).floor==5.0)) {
      ret -= 320000
      ret += 1010000
    }
    if(ret%100000000==13010000) {
      ret -= 13010000
      ret += 101010000
    }
    return ret
  }
  
  def readInfo(lines:String):Int = {
    val line = parseLine(lines)
    if(feed.size/69074.0 > readPercent) {
      readPercent += 0.01
      println("Reading... " + (readPercent*100).toInt.toString() + "%")
    }
    if(locations.contains(line(0)) && line(2).toInt <= range) {
      if(isAllDigits(line(2)) && isAllDigits(line(3)) && isAllDigits(line(4))) {
        val time = line(1).slice(3,4).toInt*100000000+line(1).slice(5,7).toInt*1000000+line(1).slice(8,10).toInt*10000+line(1).slice(11,13).toInt*100+line(1).slice(14,16).toInt
        if(time >= 6010000 && time < 105012400) {
          if(feed.size == 0) {
            feed += ((line(0),line(1),time,line(2).toInt,line(3).toInt,line(4).toInt))
            return 0
          } else if(feed.size == 1) {
            if(feed(0)._3 < time) {
              feed.insert(1, (line(0),line(1),time,line(2).toInt,line(3).toInt,line(4).toInt))
              return 1
            } else {
              feed.insert(0, (line(0),line(1),time,line(2).toInt,line(3).toInt,line(4).toInt))
              return 0
            }
          } else {
            if(feed(0)._3>time) {
              feed.insert(0, (line(0),line(1),time,line(2).toInt,line(3).toInt,line(4).toInt))
              return 0
            }
            if(feed(feed.size-1)._3<time) {
              feed.append((line(0),line(1),time,line(2).toInt,line(3).toInt,line(4).toInt))
              return feed.size-1
            }
            for(i <- 1 until feed.size) {
              if(feed(i-1)._3 < time && feed(i)._3 > time) {
                //line.foreach(x=>print(x+", "))
                feed.insert(i, (line(0),line(1),time,line(2).toInt,line(3).toInt,line(4).toInt))
                return i
              }
            }
          }
          //println("error")
          return -1
        } else {
          //println(line(1)+" out of range")
          return -2
        }
      } else {
        //println("Bad coordinate information")
        return -3
      }
    } else {
      //println(line(0)+" not in agent list")
      return -4
    }
  }
  
  def isAllDigits(x: String):Boolean = {
    for(i <- 0 until x.size) {
      if(x(i) != '0' && x(i) != '1' && x(i) != '2' && x(i) != '3' && x(i) != '4' && x(i) != '5' && x(i) != '6' && x(i) != '7' && x(i) != '8' && x(i) != '9' && x(i) != '-') {
        return false
      }
    }
    return true
  }
  
  
  def checkInRange(t:Int) {
    var year = ""
    var month = ""
    var day = ""
    var hour = ""
    var minute = ""
    if(t < 100000000) {
      year = "2010"
    } else {
      year = "2011"
    }
    if(Math.floor((t%100000000)/1000000) < 10) {
      month = "0"+Math.floor((t%100000000)/1000000).toInt.toString()
    } else {
      month = Math.floor((t%100000000)/1000000).toInt.toString()
    }
    if(Math.floor((t%1000000)/10000) < 10) {
      day = "0"+Math.floor((t%1000000)/10000).toInt.toString()
    } else {
      day = Math.floor((t%1000000)/10000).toInt.toString()
    }
    if(Math.floor((t%10000)/100) < 10) {
      hour = "0"+Math.floor((t%10000)/100).toInt.toString()
    } else {
      hour = Math.floor((t%10000)/100).toInt.toString()
    }
    if(Math.floor((t%100)) < 10) {
      minute = "0"+Math.floor(t%100).toInt.toString()
    } else {
      minute = Math.floor(t%100).toInt.toString()
    }
    val timeString = year+"-"+month+"-"+day+" "+hour+":"+minute+":00"
    locations.foreach(x => {
      locations.foreach(y => {
        if(x._1 != y._1) {
          if(x._2._1 != 20000000 && x._2._2 != 20000000 && y._2._1 != 20000000 && y._2._2 != 20000000) {
            if(x._2._1 > y._2._1) {
              if(x._2._2 > y._2._2) {
                if(Math.sqrt((x._2._1-y._2._1)*(x._2._1-y._2._1)+(x._2._2-y._2._2)*(x._2._2-y._2._2)) <= range) {
                  //println(y._1+" is in "+range+" meter range of "+x._1+"; "+x._1+":("+x._2._1+","+x._2._2+") <-> "+y._1+":("+y._2._1+","+y._2._2+")")
                  results += ((x._1,y._1,timeString))
                }
              } else {
                if(Math.sqrt((x._2._1-y._2._1)*(x._2._1-y._2._1)+(y._2._2-x._2._2)*(y._2._2-x._2._2)) <= range) {
                  //println(y._1+" is in "+range+" meter range of "+x._1+"; "+x._1+":("+x._2._1+","+x._2._2+") <-> "+y._1+":("+y._2._1+","+y._2._2+")")
                  results += ((x._1,y._1,timeString))
                }
              }
            } else {
              if(x._2._2 > y._2._2) {
                if(Math.sqrt((y._2._1-x._2._1)*(y._2._1-x._2._1)+(x._2._2-y._2._2)*(x._2._2-y._2._2)) <= range) {
                  //println(y._1+" is in "+range+" meter range of "+x._1+"; "+x._1+":("+x._2._1+","+x._2._2+") <-> "+y._1+":("+y._2._1+","+y._2._2+")")
                  results += ((x._1,y._1,timeString))
                }
              } else {
                if(Math.sqrt((y._2._1-x._2._1)*(y._2._1-x._2._1)+(y._2._2-x._2._2)*(y._2._2-x._2._2)) <= range) {
                  //println(y._1+" is in "+range+" meter range of "+x._1+"; "+x._1+":("+x._2._1+","+x._2._2+") <-> "+y._1+":("+y._2._1+","+y._2._2+")")
                  results += ((x._1,y._1,timeString))
                }
              }
            }
          }
        }
      })
    })
  }
  
  def parseLine(line:String):Array[String] = { //AUX
    line.replace("\"","").split(",")
  }
}