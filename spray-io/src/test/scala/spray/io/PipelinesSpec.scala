/*
 * Copyright (C) 2011-2012 spray.io
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package spray.io

import org.specs2.mutable.Specification
import spray.io.PipelineStage.dynamic
import spray.io.PipelineStage.Become


class PipelinesSpec extends Specification {

  sequential

  "The >> must correctly combine two PipelineStages" >> {
    val a = new TestStage('A')
    val b = new TestStage('B')
    val c = a >> b

    "example-1" in {
      test(c, TestCommand(".")) === (".AB", "")
    }
    "example-2" in {
      test(c, TestEvent(".")) === ("", ".BA")
    }
  }
  
  "A pipeline containing dynamic PipelineStages must still work normally" >> {
    val a = new TestStage('A')
    val b = new TestStage('B')
    val c = new TestStage('C')
    val stages = a >> dynamic(b) >> c
    
    "example-1" in {
      test(stages, TestCommand(".")) === (".ABC", "")
    }
    "example-2" in {
      test(stages, TestEvent(".")) === ("", ".CBA")
    }
  }
  
  "Dynamic PipelineStages must respond to Become(...) from 'outside' such that" >> {
    val a = new TestStage2('A')
    val b = new TestStage2('B')
    val c = new TestStage2('C')
    val d = new TestStage2('D')
    val one = new TestStage2('1')
    val two = new TestStage2('2')
    val stages = a >> dynamic { b >> c } >> d
    val (pl, baseResultsFunc) = prebuildPipeline(stages)
    
    "it doesn't fall through to base" in {
      test2(pl, baseResultsFunc, Become(one >> two)) === ("", "")
    }
    
    "it correctly transformed the pipeline for commands" in {
      test2(pl, baseResultsFunc, TestCommand(".")) === (".A12D", "")
    }
    
    "it correctly transformed the pipeline for events" in {
      test2(pl, baseResultsFunc, TestEvent(".")) === ("", ".D21A")
    }
  }
  
  "Dynamic PipelineStages must respond to Become(...) from 'inside' such that" >> {
    val a = new TestStage2('A')
    val b = new TestStage2('B')
    val c = new TestStage2('C')
    val one = new TestStage2('1')
    val two = new TestStage2('2')
    val x = new TestStage2('X')
    val stages = a >> dynamic { b } >> c
    val (pl, baseResultsFunc) = prebuildPipeline(stages)
    
    "it doesn't fall through to base" in {
      val command = EmitBecomeCommandFromStage('B', Become(one >> two))
      test2(pl, baseResultsFunc, command) === ("", "")
    }
    
    "it correctly transformed the pipeline for commands" in {
      test2(pl, baseResultsFunc, TestCommand(".")) === (".A12C", "")
    }
    
    "it correctly transformed the pipeline for events" in {
      test2(pl, baseResultsFunc, TestEvent(".")) === ("", ".C21A")
    }
    
    "the transformed pipeline still doesn't let new Become commands fall through" in {
      val command = EmitBecomeCommandFromStage('1', Become(x))
      test2(pl, baseResultsFunc, command) === ("", "")
    }
    
    "the transformed pipeline still handles commands correctly" in {
      test2(pl, baseResultsFunc, TestCommand(".")) === (".AXC", "")
    }
    
    "the transformed pipeline still handles events correctly" in {
      test2(pl, baseResultsFunc, TestEvent(".")) === ("", ".CXA")
    }
  }
  
  def prebuildPipeline(stage: PipelineStage) = {
    var commandResult: String = ""
    var eventResult: String = ""
    val pl = stage(
      context = null,
      commandPL = { 
        case TestCommand(s) => commandResult = s
        case Become(_) => commandResult = "BecomePassedToBase"
      },
      eventPL = { case TestEvent(s) => eventResult = s }
    )
    def getBaseResultsAndClear() = {
      val (cmdRes, evRes) = (commandResult, eventResult)
      commandResult = ""
      eventResult = ""
      (cmdRes, evRes)
    } 
    (pl, getBaseResultsAndClear _)
  }
  
  def test2(pl: Pipelines, results: () => (String, String), cmdOrEv: AnyRef) = {
    cmdOrEv match {
      case cmd: Command => pl.commandPipeline(cmd)
      case ev: Event => pl.eventPipeline(ev)
    }
    results()
  }

  def test(stage: PipelineStage, cmdOrEv: AnyRef) = {
    var commandResult: String = ""
    var eventResult: String = ""
    val pl = stage(
      context = null,
      commandPL = { case TestCommand(s) => commandResult = s },
      eventPL = { case TestEvent(s) => eventResult = s }
    )
    cmdOrEv match {
      case cmd: Command => pl.commandPipeline(cmd)
      case ev: Event => pl.eventPipeline(ev)
    }
    (commandResult, eventResult)
  }
  
  class TestStage2(c: Char) extends PipelineStage {
    def apply(context: PipelineContext, commandPL: CPL, eventPL: EPL) =
      new Pipelines {
        val commandPipeline: CPL = {
          case TestCommand(s) => commandPL(TestCommand(s + c))
          case EmitBecomeCommandFromStage(ch, b) if c == ch => commandPL(b)
          case cmd => commandPL(cmd)
        }
        val eventPipeline: EPL = {
          case TestEvent(s) => eventPL(TestEvent(s + c))
        }
      }
  }

  class TestStage(c: Char) extends PipelineStage {
    def apply(context: PipelineContext, commandPL: CPL, eventPL: EPL) =
      new Pipelines {
        val commandPipeline: CPL = {
          case TestCommand(s) => commandPL(TestCommand(s + c))
        }
        val eventPipeline: EPL = {
          case TestEvent(s) => eventPL(TestEvent(s + c))
        }
      }
  }

  case class TestEvent(s: String) extends Event
  case class TestCommand(s: String) extends Command
  case class EmitBecomeCommandFromStage(ch: Char, b: Become) extends Command
}
