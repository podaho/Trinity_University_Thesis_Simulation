package Simulation

import collection.mutable.HashMap
import scala.collection.mutable.Buffer

class DataTable {
  var table = Buffer[Buffer[Buffer[(String, String, Int, Int, Double, Int, Int)]]]() //(Agent i, Agent j, FW, numCalls, intLen, numSMS, contact minutes)
  var agents = HashMap[String, Int]()
  var agents2 = HashMap[Int, String]()
  var numAgents:Int = 0
  var numSMSRecords = 0
  var numCallRecords = 0
  var numFWRecords = 0
  
  //var agentANotFound = 0
  //var agentBEmpty = 0
  //var agentBNotFound = 0
  //var missingRecordDate = 0
  //var recordDateOutOfRange = 0
  //var emptyCallType = 0
  //var badCallType = 0
  //var missingDurationInfo = 0
  //var used = 0
  var earliestCall = "2012-01-01"
  var earliestSMS = "2012-01-01"
  
  
  
  def checkValid(agnt1:String,agnt2:String):Boolean = {
    if(fetchFW(agnt1,agnt2,"2010-09-01") != 10 && fetchFW(agnt1,agnt2,"2010-12-01") != 10 && fetchFW(agnt1,agnt2,"2011-03-01") != 10 && fetchFW(agnt1,agnt2,"2011-05-01") != 10) {
      return true
    } else {
      return false
    }
  }
  
  def fetchContactLengthTotal(agnt:String, agnt2:String):Int = {
    var ret = 0
    table.foreach(i => {
      ret += i(agents(agnt))(agents(agnt2))._7 
    })
    return ret
  }
  
  def fetchContactLength(agnt:String, agnt2:String, time:Int):Int = {
    var ret = 0
    if(time == 1) {
      for(i <- 0 until findTimeStep("2010-09-01")) {
        ret += table(i)(agents(agnt))(agents(agnt2))._7
      }
    }
    if(time == 2) {
      for(i <- findTimeStep("2010-09-01") until findTimeStep("2010-12-01")) {
        ret += table(i)(agents(agnt))(agents(agnt2))._7
      }
    }
    if(time == 3) {
      for(i <- findTimeStep("2010-12-01") until findTimeStep("2011-03-01")) {
        ret += table(i)(agents(agnt))(agents(agnt2))._7
      }
    }
    if(time == 4) {
      for(i <- findTimeStep("2011-03-01") until findTimeStep("2011-05-01")) {
        ret += table(i)(agents(agnt))(agents(agnt2))._7
      }
    }
    return ret
  }
  
  def fetchFW(agnt:String, agnt2:String, date:String):Int = {
    val i1 = findTimeStep(date)
    val i2 = agents(agnt)
    val i3 = agents(agnt2)
    return table(i1)(i2)(i3)._3
  }
  
  def fetchFWTrend(source:String, target:String, rval:Double):(String,String,Double,Int,Int,Int,Int) = {
    val res1 = fetchFW(source,target,"2010-09-01")
    val res2 = fetchFW(source,target,"2010-12-01")
    val res3 = fetchFW(source,target,"2011-03-01")
    val res4 = fetchFW(source,target,"2011-05-01")
    
    return (source,target,rval,res1,res2,res3,res4)
  }
  
  def readAgents(input:String) { //STEP 1
    val line = parseLine(input)
    if(!line(1).isEmpty()) {
      setAgents(line(0))
      setAgents(line(1))
    } else {
      setAgents(line(0))
    }
  }
  
  def setAgents(id:String) { //AUX
    if(!agents.contains(id)) {
      agents += (id -> numAgents)
      agents2 += (numAgents -> id)
      numAgents += 1
    }
  }
  
  def createEmpty() { //STEP 2
    var atomic = ("","",0,0,0.0,0,0)
    table = Buffer.fill(336, numAgents, numAgents)(atomic.copy())
    for(t <- 0 until 336) {  
      if(t%50==0) println("Creating Empty Table...")
      for(i <- 0 until numAgents) {
        for(j <- 0 until numAgents) {
          table(t)(i)(j) = (agents2(i), agents2(j), 10, 0, 0.0, 0, 0)
        }
      }
    }
  }
  
  def writeFW(input:String) { //STEP 3-1: Layout for debug purposes
    if(numFWRecords % 1000 == 0) println("Reading Friendship Weight Records...")
    val line = parseLine(input)
    if(agents.contains(line(1))) { 
      if(!line(2).isEmpty() && agents.contains(line(2))) {
        if(line(4) == "2010-09-01") {
          numFWRecords += 1
          for(i <- findTimeStep("2010-09-01") to findTimeStep("2010-12-01")-1) {
            val ret = table(i)(agents(line(1)))(agents(line(2)))
            if(ret._1 == line(1) && ret._2 == line(2)) {
              table(i)(agents(line(1)))(agents(line(2))) = (line(1),line(2),line(3).toInt,ret._4,ret._5,ret._6, ret._7)
            } else {
              println("NOT MATCHING AGENT IN FW WRITE TO TABLE")
            }
          }
        }
        if(line(4) == "2010-12-01") {
          numFWRecords += 1
          for(i <- findTimeStep("2010-12-01") to findTimeStep("2011-03-01")-1) {
            val ret = table(i)(agents(line(1)))(agents(line(2)))
            if(ret._1 == line(1) && ret._2 == line(2)) {
              table(i)(agents(line(1)))(agents(line(2))) = (line(1),line(2),line(3).toInt,ret._4,ret._5,ret._6,ret._7)
            } else {
              println("NOT MATCHING AGENT IN FW WRITE TO TABLE")
            }
          }
        }
        if(line(4) == "2011-03-01") {
          numFWRecords += 1
          for(i <- findTimeStep("2011-03-01") to findTimeStep("2011-05-01")-1) {
            val ret = table(i)(agents(line(1)))(agents(line(2)))
            if(ret._1 == line(1) && ret._2 == line(2)) {
              table(i)(agents(line(1)))(agents(line(2))) = (line(1),line(2),line(3).toInt,ret._4,ret._5,ret._6,ret._7)
            } else {
              println("NOT MATCHING AGENT IN FW WRITE TO TABLE")
            }
          }
        }
        if(line(4) == "2011-05-01") {
          numFWRecords += 1
          for(i <- findTimeStep("2011-05-01") to findTimeStep("2011-05-02")-1) {
            val ret = table(i)(agents(line(1)))(agents(line(2)))
            if(ret._1 == line(1) && ret._2 == line(2)) {
              table(i)(agents(line(1)))(agents(line(2))) = (line(1),line(2),line(3).toInt,ret._4,ret._5,ret._6,ret._7)
            } else {
              println("NOT MATCHING AGENT IN FW WRITE TO TABLE")
            }
          }
        }
      }
    }
  }
  
  def writeCall(input:String) { //STEP 3-2: Layout for debug purposes
    if(numCallRecords % 1000 == 0) println("Reading Call Records...")
    val line = parseLine(input)
    if(agents.contains(line(0))) {  
      if(!line(1).isEmpty() && agents.contains(line(1))) {
        if(!line(2).isEmpty() && timeCompare(line(2), "2010-06-01") >= 0 && timeCompare(line(2), "2011-05-02") == -1) {
          if(!line(3).isEmpty() && line(3)(0) == 'o') {
            if(!line(4).isEmpty()) {
              val ret = table(findTimeStep(line(2))-1)(agents(line(0)))(agents(line(1)))
              if(ret._1 == line(0) && ret._2 == line(1)) {
                numCallRecords += 1
                table(findTimeStep(line(2))-1)(agents(line(0)))(agents(line(1))) = (line(0), line(1), ret._3, ret._4+1, ret._5+(line(4).toDouble/60), ret._6, ret._7)
                //used += 1
                if(timeCompare(line(2),earliestCall) == -1) {
                  earliestCall = line(2)
                }
              } else {
                println("NOT MATCHING AGENT IN CALL WRITE TO TABLE")
              }
            }
          }
          if(!line(3).isEmpty() && line(3)(0) == 'i') {
            if(!line(4).isEmpty()) {
              val ret = table(findTimeStep(line(2))-1)(agents(line(0)))(agents(line(1)))
              if(ret._1 == line(0) && ret._2 == line(1)) {
                numCallRecords += 1
                table(findTimeStep(line(2))-1)(agents(line(0)))(agents(line(1))) = (line(0), line(1), ret._3, ret._4, ret._5+(line(4).toDouble/60), ret._6, ret._7)
                //used += 1
              } else {
                println("NOT MATCHING AGENT IN CALL WRITE TO TABLE")
              }
            }
          }
        }
      }
    } /*else {
      agentANotFound += 1
    }
    if(line(3).isEmpty()) {
      emptyCallType += 1
    } else {
      if(line(3)(0) != 'i' && line(3)(0) != 'o') {
        badCallType += 1       
      }
    }
    if(line(4).isEmpty()) {
      missingDurationInfo += 1
    }
    if(timeCompare(line(2), "2010-06-01") == -1 || timeCompare(line(2), "2011-05-02") > 0) {
      recordDateOutOfRange += 1
    }
    if(line(1).isEmpty) {
      agentBEmpty += 1
    } else {
      if(!agents.contains(line(1))) {
        agentBNotFound += 1
      }
    }*/
  }

  def writeSMS(input:String) { //STEP 3-3: Layout for debug purposes
    if(numSMSRecords % 1000 == 0) println("Reading SMS Records...")
    val line = parseLine(input)
    if(agents.contains(line(0))) {  
      if(!line(1).isEmpty() && agents.contains(line(1))) {
        if(!line(2).isEmpty() && timeCompare(line(2), "2010-06-01") >= 0 && timeCompare(line(2), "2011-05-02") == -1) {
          if(!line(3).isEmpty() && line(3)(0) == 'o') {
            val ret = table(findTimeStep(line(2))-1)(agents(line(0)))(agents(line(1)))
            if(ret._1 == line(0) && ret._2 == line(1)) {
              numSMSRecords += 1
              table(findTimeStep(line(2))-1)(agents(line(0)))(agents(line(1))) = (line(0), line(1), ret._3, ret._4, ret._5, ret._6 + 1, ret._7)
              //used += 1
              if(timeCompare(line(2),earliestSMS) == -1) {
                earliestSMS = line(2)
              }
            } else {
              println("NOT MATCHING AGENT IN SMS WRITE TO TABLE")
            }
          }
        }
      }
    }
    /*if(!agents.contains(line(0))) {
      agentANotFound += 1
    }
    if(line(1).isEmpty()) {
      agentBEmpty += 1
    } else {
      if(!agents.contains(line(1))) {
        agentBNotFound += 1
      }
    }
    if(line(2).isEmpty()) {
      missingRecordDate += 1
    } else {
      if(timeCompare(line(2), "2010-06-01") == -1 || timeCompare(line(2), "2011-05-02") > 0) {
        recordDateOutOfRange += 1
      }
    }
    if(line(3).isEmpty()) {
      emptyCallType += 1
    } else {
      if(line(3)(0) != 'o') {
        badCallType += 1
      }
    }*/
  }
  
  def writeContact(input:Buffer[(String,String,String)]) = {
    input.foreach(x => {
      if(agents.contains(x._1) && agents.contains(x._2)) {
        val ret = table(findTimeStep(x._3)-1)(agents(x._1))(agents(x._2))
        table(findTimeStep(x._3)-1)(agents(x._1))(agents(x._2)) = (ret._1, ret._2, ret._3, ret._4, ret._5, ret._6, ret._7+1)
      }
    })
  }
  
  
  def findTimeStep(input:String):Int = { //AUX: input must be within date range 2010-06-01 ~ 2011-05-01
    val june10 = 30
    val july10 = 31
    val aug10 = 31
    val sept10 = 30
    val oct10 = 31
    val nov10 = 30
    val dec10 = 31
    val jan11 = 31
    val feb11 = 28
    val march11 = 31
    val apr11 = 30
    val may11 = 1
    var year = input.slice(0,4).toInt
    var month = input.slice(5,7).toInt
    var day = input.slice(8,10).toInt
    //var hour = input.slice(11,13).toInt
    if(year < 2011) month match {
      case 6 => {
        return (day)
      }
      case 7 => {
        return (june10+day)
      }
      case 8 => {
        return (july10 + june10 + day)
      }
      case 9 => {
        return (aug10 + july10 + june10 + day)
      }
      case 10 => {
        return (sept10 + aug10 + july10 + june10 + day)
      }
      case 11 => {
        return (oct10 + sept10 + aug10 + july10 + june10 + day)
      }
      case 12 => {
        return (nov10 + oct10 + sept10 + aug10 + july10 + june10 + day)
      }
    } else {
      val retval = dec10 + nov10 + oct10 + sept10 + aug10 + july10 + june10
      month match {
        case 1 => {
          return (retval + day)
        }
        case 2 => {
          return (retval + jan11 + day)
        }
        case 3 => {
          return (retval + feb11 + jan11 + day)
        }
        case 4 => {
          return (retval + march11 + feb11 + jan11 + day)
        }
        case 5 => {
          return (retval + apr11 + march11 + feb11 + jan11 + day)
        }
      }
    }
  }
  
  def parseLine(line:String):Array[String] = { //AUX
    line.replace("\"","").split(",")
  }
  
  def timeCompare(x:String,y:String):Int = { //AUX
    if(x.size == 19 && y.size == 19) {
      var yearx = x.slice(0,4).toInt
      var monthx = x.slice(5,7).toInt
      var dayx = x.slice(8,10).toInt
      var hourx = x.slice(11,13).toInt
      var minutex = x.slice(14,16).toInt
      var secondx = x.slice(17,19).toInt
      var yeary = y.slice(0,4).toInt
      var monthy = y.slice(5,7).toInt
      var dayy = y.slice(8,10).toInt
      var houry = y.slice(11,13).toInt
      var minutey = y.slice(14,16).toInt
      var secondy = y.slice(17,19).toInt
      if(yearx < yeary) {
        return -1
      } else if(monthx < monthy && yearx == yeary) {
        return -1
      } else if(dayx < dayy && monthx == monthy && yearx == yeary) {
        return -1
      } else if(hourx < houry && dayx == dayy && monthx == monthy && yearx == yeary) {
        return -1
      } else if(minutex < minutey && hourx == houry && dayx == dayy && monthx == monthy && yearx == yeary) {
        return -1
      } else if(secondx < secondy && minutex == minutey && hourx == houry && dayx == dayy && monthx == monthy && yearx == yeary) {
        return -1
      } else if(secondx == secondy && minutex == minutey && hourx == houry && dayx == dayy && monthx == monthy && yearx == yeary) {
        return 0
      } else {
        return 1
      }
    } else {
      var yearx = x.slice(0,4).toInt
      var monthx = x.slice(5,7).toInt
      var dayx = x.slice(8,10).toInt
      var yeary = y.slice(0,4).toInt
      var monthy = y.slice(5,7).toInt
      var dayy = y.slice(8,10).toInt
      if(yearx < yeary) {
        return -1
      } else if(monthx < monthy && yearx == yeary) {
        return -1
      } else if(dayx < dayy && monthx == monthy && yearx == yeary) {
        return -1
      } else if(dayx == dayy && monthx == monthy && yearx == yeary) {
        return 0
      } else {
        return 1
      }
    }
  }
}