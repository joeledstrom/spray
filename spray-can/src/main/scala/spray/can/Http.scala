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

package spray.can

import java.net.InetSocketAddress
import com.typesafe.config.Config
import scala.collection.immutable
import akka.util.ByteString
import akka.io.Tcp
import akka.actor._
import spray.can.server.{ServerFrontend, ServerSettings}
import spray.can.client.ClientSettings
import spray.io.{ConnectionTimeouts, ClientSSLEngineProvider, ServerSSLEngineProvider}
import spray.http.{HttpMessagePart, HttpMessagePartWrapper}

object Http extends ExtensionKey[HttpExt] {

  /// COMMANDS
  type Command = Tcp.Command

  case class Connect(remoteAddress: InetSocketAddress,
                     localAddress: Option[InetSocketAddress] = None,
                     options: immutable.Traversable[Tcp.SocketOption] = Nil,
                     settings: Option[ClientSettings] = None)
                    (implicit val sslEngineProvider: ClientSSLEngineProvider)extends Command

  case class Bind(handler: ActorRef,
                  endpoint: InetSocketAddress,
                  backlog: Int = 100,
                  options: immutable.Traversable[Tcp.SocketOption] = Nil,
                  settings: Option[ServerSettings] = None)
                 (implicit val sslEngineProvider: ServerSSLEngineProvider) extends Command

  type Register = Tcp.Register; val Register = Tcp.Register

  type CloseCommand = Tcp.CloseCommand
  val Close = Tcp.Close
  val ConfirmedClose = Tcp.ConfirmedClose
  val Abort = Tcp.Abort

  case object ClearStats extends Command
  case object GetStats extends Command

  type SetIdleTimeout = ConnectionTimeouts.SetIdleTimeout; val SetIdleTimeout = ConnectionTimeouts.SetIdleTimeout

  // only for server connections
  type SetRequestTimeout = ServerFrontend.SetRequestTimeout;  val SetRequestTimeout = ServerFrontend.SetRequestTimeout
  type SetTimeoutTimeout = ServerFrontend.SetTimeoutTimeout;  val SetTimeoutTimeout = ServerFrontend.SetTimeoutTimeout

  case class MessageCommand(cmd: HttpMessagePartWrapper) extends Command

  private[can] case class Write(data: ByteString,
                                ack: Any,
                                part: HttpMessagePart,
                                requestSender: ActorRef) extends Tcp.Write {
    def copy(data: ByteString = data, ack: Any = ack): Tcp.Write = Write(data, ack, part, requestSender)
  }


  /// EVENTS
  type Event = Tcp.Event

  type Connected = Tcp.Connected; val Connected = Tcp.Connected
  val Bound = Tcp.Bound
  val Unbound = Tcp.Unbound
  type ConnectionClosed = Tcp.ConnectionClosed

  val Closed = Tcp.Closed
  val Aborted = Tcp.Aborted
  val ConfirmedClosed = Tcp.ConfirmedClosed
  val PeerClosed = Tcp.PeerClosed
  type ErrorClosed = Tcp.ErrorClosed; val ErrorClosed = Tcp.ErrorClosed

  case class SendFailed(part: HttpMessagePart) extends Event
  case class MessageEvent(ev: HttpMessagePart) extends Event
}

class HttpExt(system: ExtendedActorSystem) extends akka.io.IO.Extension {

  val Settings = new Settings(system.settings.config getConfig "spray.can")
  class Settings private[HttpExt] (config: Config) {
    val ManagerDispatcher = config getString "manager-dispatcher"
    val ClientDispatcher = config getString "client-dispatcher"
    val ListenerDispatcher = config getString "listener-dispatcher"
    val ConnectionDispatcher = config getBoolean "connection-dispatcher"
  }

  val manager = system.actorOf(
    props = Props(new HttpManager(Settings)) withDispatcher Settings.ManagerDispatcher,
    name = "IO-HTTP")
}
