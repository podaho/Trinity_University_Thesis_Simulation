package Simulation

import collection.mutable.HashMap
import scala.collection.mutable.Buffer
import io.StdIn._
import scala.io.Source
import java.io._
import scala.math._

object main {
  val initCost:Double = 0.1
  val costPerMinute:Double = 0.02
  val timeDiscountFactorCall:Double = 0.8
  val costPerMessage:Double = 0.02
  val timeDiscountFactorSMS:Double = 0.6
  val upperBound = 50.0
  val lowerBound = 0.0
  
  var networks = Buffer[Network]()
  var data = new DataTable
  var locGrid = new LocationGrid
  var agentList = Buffer[String]()
  var memManage = HashMap[String,HashMap[String,(Double,Double,Int)]]()
  var learningTables = HashMap[String,(QTable,QTable)]()
  var results = HashMap[String, (Buffer[(String,Double,Double,Double,Double)],Buffer[(String,Double,Double,Double,Double)])]()
  var seenCallValues = Buffer[Double]()
  var seenSMSValues = Buffer[Double]()
  
  var trendCall = Buffer[(String,String,Double,Int,Int,Int,Int)]()
  var lowerTrendCall = Buffer[(String,String,Double,Int,Int,Int,Int)]()
  var middleTrendCall = Buffer[(String,String,Double,Int,Int,Int,Int)]()
  var upperTrendCall = Buffer[(String,String,Double,Int,Int,Int,Int)]()
  
  var trendSMS = Buffer[(String,String,Double,Int,Int,Int,Int)]()
  var lowerTrendSMS = Buffer[(String,String,Double,Int,Int,Int,Int)]()
  var middleTrendSMS = Buffer[(String,String,Double,Int,Int,Int,Int)]()
  var upperTrendSMS = Buffer[(String,String,Double,Int,Int,Int,Int)]()
  
  var stdDevOffsetCall = 0.0
  var stdDevOffsetSMS = 0.0
  
  def main(args:Array[String]) {  
    val bufferSourceReadAgents = Source.fromFile("SMSLog.csv")
    var readAgentsLineCount = 0
    for(line <- bufferSourceReadAgents.getLines) {
      if(readAgentsLineCount == 0) {
        readAgentsLineCount += 1
      } else {
        data.readAgents(line)
        readAgentsLineCount += 1
      }
    }
    bufferSourceReadAgents.close()
    
    data.createEmpty()
    
    val bufferSourceSMS = Source.fromFile("SMSLog.csv")
    var sMSLineCount = 0
    for(line <- bufferSourceSMS.getLines) {
      if(sMSLineCount == 0) {
        sMSLineCount += 1
      } else {
        data.writeSMS(line)
        sMSLineCount += 1
      }
    }
    bufferSourceSMS.close()
    
    val bufferSourceCall = Source.fromFile("CallLog.csv")
    var callLineCount = 0
    for(line <- bufferSourceCall.getLines) {
      if(callLineCount != 0) {
        data.writeCall(line)
        callLineCount += 1
      } else {
        callLineCount += 1
      }
    }
    bufferSourceCall.close()    
    
    val bufferSourceFW = Source.fromFile("SurveyFriendship.csv")
    var fwLineCount = 0
    for(line <- bufferSourceFW.getLines) {
      if(fwLineCount != 0) {
        data.writeFW(line)
        fwLineCount += 1
      } else {
        fwLineCount += 1
      }
    }
    bufferSourceFW.close()
    
    data.agents.foreach(x => {
      agentList += x._1
      learningTables += (x._1 -> (new QTable, new QTable))
    })
    
    locGrid.setAgents(agentList)
    
    val bufferSourceL = Source.fromFile("Location.csv")
    var locationLineCount = 0
    for(line <- bufferSourceL.getLines) {
      if(locationLineCount != 0) {
        locGrid.readInfo(line)
        locationLineCount += 1
      } else {
        locationLineCount += 1
      }
    }
    bufferSourceL.close()
    println("size of feed = "+locGrid.feed.size)
    
    locGrid.process()
    
    data.writeContact(locGrid.results)
    
    
    
    ////////////////////////////////////////NETWORKS START/////////////////////////////////////////////
    
    var firstNetwork = new Network(0, costPerMinute, initCost, costPerMessage, timeDiscountFactorCall, timeDiscountFactorSMS)
    firstNetwork.createAgents(agentList)
    
    firstNetwork.updateNetwork(false, false, data.table(0))
    
    memManage = firstNetwork.memory
    
    for(i <- 1 to data.findTimeStep("2011-05-01")) {
      println("Learning Network #" + i)
      var network = new Network(i, costPerMinute, initCost, costPerMessage, timeDiscountFactorCall, timeDiscountFactorSMS)
      network.createAgents(agentList)
      network.load(memManage)
      if(i == data.findTimeStep("2010-09-01 23:00:00") || i == data.findTimeStep("2010-12-01 23:00:00") || i == data.findTimeStep("2011-03-01 23:00:00")) {
        network.updateNetwork(false, true, data.table(i))
        network.learningTable.foreach(x => {
          if(data.checkValid(x._1.getID(), x._2.getID())) {
            learningTables(x._1.getID())._1.writeValue(x._3, x._5) //CHANGED TO ONLY WRITING VALUES OTHER THAN Rval=0.0 FWOffset=0 SINCE THIS HAS NO MEANING
            if(x._3 != 0) println(x._1.getID() + " writing RvalCall: " + x._3 + ", FW: " + x._5)
            learningTables(x._1.getID())._2.writeValue(x._4, x._5) //CHANGED TO ONLY WRITING VALUES OTHER THAN Rval=0.0 FWOffset=0 SINCE THIS HAS NO MEANING
            if(x._4 != 0) println(x._1.getID() + " writing RvalSMS: " + x._4 + ", FW: " + x._5)
          }
        })
      } else if(i == data.findTimeStep("2011-05-01 23:00:00")) {
        network.updateNetwork(true, false, data.table(i))
        network.simulationTable.foreach(x => {
          //(source agent, target agent, Rval Call, Rval SMS, FW offset)
          if(data.checkValid(x._1.getID(), x._2.getID())) {
            val res1 = x._5.toDouble-learningTables(x._1.getID())._1.fetchValue(x._3)
            val res2 = x._5.toDouble-learningTables(x._1.getID())._2.fetchValue(x._4)
            if(results.contains(x._1.getID())) {
              results(x._1.getID)._1 += ((x._2.getID(),x._5,x._3,learningTables(x._1.getID())._1.fetchValue(x._3),res1))
              results(x._1.getID)._2 += ((x._2.getID(),x._5,x._4,learningTables(x._1.getID())._2.fetchValue(x._4),res2))
            } else {
              results += (x._1.getID -> (Buffer[(String,Double,Double,Double,Double)](),Buffer[(String,Double,Double,Double,Double)]()))
              results(x._1.getID)._1 += ((x._2.getID(),x._5,x._3,learningTables(x._1.getID())._1.fetchValue(x._3),res1))
              results(x._1.getID)._2 += ((x._2.getID(),x._5,x._4,learningTables(x._1.getID())._2.fetchValue(x._4),res2))
            }
          }
        })
      } else {
        network.updateNetwork(false, false, data.table(i))
      }
      memManage = network.memory
    }
    
    /*learningTables.foreach(x => {
      val writer1 = new PrintWriter(new File(x._1+"_Call.csv"))
      x._2._1.table.foreach(y => {
        y._2.foreach(z => {
          writer1.write(y._1.toString()+","+z.toString()+"\n")
        })
      })
      writer1.close()
      val writer2 = new PrintWriter(new File(x._1+"_SMS.csv"))
      x._2._2.table.foreach(y => {
        y._2.foreach(z => {
          writer2.write(y._1.toString()+","+z.toString()+"\n")
        })
      })
      writer2.close()
    })*/
    
    var tmp1 = 0.0
    var c1 = 0
    val writer3 = new PrintWriter(new File("ResultsCall.csv"))
    writer3.write("CPM,"+costPerMinute.toString()+"\n")
    writer3.write("TimeDiscount,"+timeDiscountFactorCall.toString()+"\n")
    results.foreach(x => {
      x._2._1.foreach(y => {
        writer3.write(x._1+","+y._1+","+y._2.toString()+","+y._3.toString()+","+y._4.toString()+","+y._5.toString()+"\n")
        seenCallValues += (y._3)
        trendCall += (data.fetchFWTrend(x._1,y._1,y._3))
        tmp1 += y._5
        c1 += 1
      })
    })
    writer3.close()
    var sumDiff1 = 0.0
    results.foreach(x => {
      x._2._1.foreach(y => {
        val mean = tmp1.toDouble/c1
        sumDiff1 += (y._5-mean)*(y._5-mean)
        stdDevOffsetCall = sqrt(sumDiff1.toDouble/c1)
      })
    })
    
    var tmp2 = 0.0
    var c2 = 0
    val writer4 = new PrintWriter(new File("ResultsSMS.csv"))
    writer4.write("CPMs,"+costPerMessage.toString()+"\n")
    writer4.write("TimeDiscount,"+timeDiscountFactorSMS.toString()+"\n")
    results.foreach(x => {
      x._2._2.foreach(y => {
        writer4.write(x._1+","+y._1+","+y._2.toString()+","+y._3.toString()+","+y._4.toString()+","+y._5.toString()+"\n")
        seenSMSValues += (y._3)
        trendSMS += (data.fetchFWTrend(x._1,y._1,y._3))
        tmp2 += y._5
        c2 += 1
      })
    })
    writer4.close()
    var sumDiff2 = 0.0
    results.foreach(x => {
      x._2._2.foreach(y => {
        val mean = tmp2.toDouble/c2
        sumDiff2 += (y._5-mean)*(y._5-mean)
        stdDevOffsetSMS = sqrt(sumDiff2.toDouble/c2)
      })
    })
    
    val writer5 = new PrintWriter(new File("RValCall.csv"))
    seenCallValues.foreach(x => {
      writer5.write(x.toString()+"\n")
    })
    writer5.close()
 
    val writer6 = new PrintWriter(new File("RValSMS.csv"))
    seenSMSValues.foreach(x => {
      writer6.write(x.toString()+"\n")
    })
    writer6.close()
    
    val writer13 = new PrintWriter(new File("TrendCall.csv"))
    trendCall.foreach(x => {
      writer13.write(x._1+","+x._2+","+x._3+","+x._4.toString()+","+x._5.toString()+","+x._6.toString()+","+x._7.toString()+","+data.fetchContactLengthTotal(x._1, x._2)+","+data.fetchContactLength(x._1,x._2,1)+","+data.fetchContactLength(x._1,x._2,2)+","+data.fetchContactLength(x._1,x._2,3)+","+data.fetchContactLength(x._1,x._2,4)+"\n")
    })
    writer13.close()
    
    val writer14 = new PrintWriter(new File("TrendSMS.csv"))
    trendSMS.foreach(x => {
      writer14.write(x._1+","+x._2+","+x._3+","+x._4.toString()+","+x._5.toString()+","+x._6.toString()+","+x._7.toString()+","+data.fetchContactLengthTotal(x._1, x._2)+","+data.fetchContactLength(x._1,x._2,1)+","+data.fetchContactLength(x._1,x._2,2)+","+data.fetchContactLength(x._1,x._2,3)+","+data.fetchContactLength(x._1,x._2,4)+"\n")
    })
    writer14.close()
    
    println(data.earliestCall)
    println(data.earliestSMS)
    
    query(tmp1.toDouble/c1,tmp2.toDouble/c2,stdDevOffsetCall,stdDevOffsetSMS)
  }
  
  def query(x:Double,y:Double,sdoc:Double,sdos:Double) {
    println("CPM: "+costPerMinute)
    println("TDC: "+timeDiscountFactorCall)
    println("Call Offset Average: "+x)
    println("Call Offset Average StdDev: "+sdoc)
    println("Call: "+x*sdoc)
    println("CPMs: "+costPerMessage)
    println("TDS: "+timeDiscountFactorSMS)
    println("SMS Offset Average: "+y)
    println("SMS Offset Average StdDev: "+sdos)
    println("SMS: "+y*sdos)
    println("Input Agent ID: ")
    val input = readLine()
    println("Select: 1) Call  2) SMS")
    val option = readLine()
    if(option == "1") {
      println(input)
      learningTables(input)._1.prettyPrint()
    } else if(option == "2") {
      println(input)
      learningTables(input)._2.prettyPrint()
    } else {
      println("Bad Selection")
    }
    query(x,y,sdoc,sdos)
  }
}