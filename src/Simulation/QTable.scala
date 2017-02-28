package Simulation

import collection.mutable.HashMap
import scala.collection.mutable.Buffer

class QTable {
  var table = HashMap[Double, Buffer[Int]]()
  var seenValues = Buffer[Double]()
  
  def prettyPrint() {
    seenValues.sorted
    seenValues.foreach(x=>{
      println(x+": Avg = "+table(x).sum+"/"+table(x).size+" = "+table(x).sum.toDouble/table(x).size+": ")
      table(x).sorted
      for(i <- 0 until 10) {
        println("   FW "+i+": "+table(x).count(x => x==i)+" occurances")
      }
      println()
    })
  }
  
  def writeValue(rVal:Double, fwChange:Int) {
    if(table.contains(rVal)) {
      table(rVal) += fwChange
    } else {
      table += (rVal -> Buffer[Int]())
      table(rVal) += fwChange
      seenValues += rVal
    }
  }
  
  def fetchValue(rVal:Double):Double = {
    if(table.contains(rVal)) {
      return table(rVal).sum.toDouble/table(rVal).size
    } else {
      if(seenValues.size >= 2) {
        var upper = Double.MaxValue
        var upperer = Double.MaxValue
        var lower = -100.0
        var lowerer = -100.0
        var tester = -2
        seenValues.foreach(x => {
          if(x>rVal && x<upper) {
            upperer = upper
            upper = x
          } else if(x>upper && x<upperer) {
            upperer = x
          }
          if(x<rVal && x>lower) {
            lowerer = lower
            lower = x
          } else if(x<lower && x>lowerer) {
            lowerer = x
          }
        })
        if(table.contains(upper) && table.contains(upperer)) tester = 1
        if(table.contains(lower) && table.contains(lowerer)) tester = -1
        if(table.contains(lower) && table.contains(upper)) tester = 0
        if(tester == 1) {
          return estimate(upper,table(upper).sum.toDouble/table(upper).size,upperer,table(upperer).sum.toDouble/table(upperer).size,rVal)
        } else if(tester == -1) {
          return estimate(lowerer,table(lowerer).sum.toDouble/table(lowerer).size,lower,table(lower).sum.toDouble/table(lower).size,rVal)
        } else if(tester == 0) {
          return estimate(lower,table(lower).sum.toDouble/table(lower).size,upper,table(upper).sum.toDouble/table(upper).size,rVal)
        } else {
          if(table.contains(lower)) {
            return table(lower).sum.toDouble/table(lower).size
          } else {
            if(table(upper).size==0) {
              return 0
            } else {
              return table(upper).sum.toDouble/table(upper).size
            }
          }
        }
      } else {
        if(table(seenValues(0)).size==0) {
          return 0
        } else {
          return table(seenValues(0)).sum.toDouble/table(seenValues(0)).size
        }
      }
    }
  }
  
  def estimate(val11:Double,val12:Double,val21:Double,val22:Double,input:Double):Double = {
    val m = (val12-val22)/(val11-val21)
    val b = val12.toDouble-(m*val11)
    if((m*input+b).isNaN()) {
      return 0.0
    } else {
      return (m*input+b)
    }
  }
}