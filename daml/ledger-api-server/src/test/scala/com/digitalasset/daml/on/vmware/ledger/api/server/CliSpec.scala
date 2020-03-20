package com.digitalasset.daml.on.vmware.ledger.api.server

import java.io.File
import java.net.InetSocketAddress
import java.nio.file.{Files, Paths, StandardCopyOption}
import java.security.KeyPairGenerator
import java.security.interfaces.{RSAPrivateKey, RSAPublicKey}

import com.digitalasset.daml.on.vmware.ledger.api.server.VDAMLRunnerMain.KVBCLedgerFactory
import com.digitalasset.jwt.domain.DecodedJwt
import com.digitalasset.jwt.{JwtSigner, KeyUtils, domain}
import com.digitalasset.ledger.api.auth.{AuthService, AuthServiceJWT, ClaimPublic}
import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}
import io.grpc.Metadata
import org.scalatest.{Assertion, AsyncWordSpec, Matchers}

import scalaz.syntax.show._
import scalaz.\/

import scala.compat.java8.FutureConverters._
import scala.concurrent.Future

/** Helper to create a HTTP server that serves a constant response on the "/result" URL */
private object SimpleHttpServer {
  def start(response: String): HttpServer = {
    val server = HttpServer.create(new InetSocketAddress(0), 0)
    server.createContext("/result", new HttpResultHandler(response))
    server.setExecutor(null)
    server.start()
    server
  }

  def responseUrl(server: HttpServer) =
    s"http://localhost:${server.getAddress.getPort}/result"

  def stop(server: HttpServer): Unit =
    server.stop(0)

  private[this] class HttpResultHandler(response: String) extends HttpHandler {
    def handle(t: HttpExchange): Unit = {
      t.sendResponseHeaders(200, response.getBytes().length.toLong)
      val os = t.getResponseBody
      os.write(response.getBytes)
      os.close()
    }
  }
}

class CliSpec extends AsyncWordSpec with Matchers {
  "Cli" should {

    "return the default Config when no arguments are specified" in {
      val config = Cli.parse(Array.empty)
      config shouldEqual Some(Config.default)
    }

    "parse and configure the authorisation mechanism correctly when `--auth-jwt-hs256-unsafe someSecret` is passed" in {
      val config = Cli.parse(Array("--auth-jwt-hs256-unsafe", "someSecret"))
      config shouldBe defined
      val authService = getAuthService(config)
      val metadata = getAuthMetadata("eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJodHRwczovL2RhbWwuY29tL2xlZGdlci1hcGkiOnt9LCJleHAiOjE5MDA4MTkzODB9.kNA5SrV4HUR3BwligGSMcpOG9bPOyHVwpNYb3Ha5dPY")
      decodeAndCheckMetadata(authService, metadata)
    }

    "parse and configure the authorisation mechanism correctly when `--auth-jwt-rs256-crt <PK.crt>` is passed" in {
      val tmpCrtFile = getTmpCrtFileAbsolutePath("/test.pubk.rsa.crt")
      val config = Cli.parse(Array("--auth-jwt-rs256-crt", tmpCrtFile))
      config shouldBe defined
      val authService = getAuthService(config)
      val metadata = getAuthMetadata("eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJodHRwczovL2RhbWwuY29tL2xlZGdlci1hcGkiOnt9LCJleHAiOjE5MDA4MTkzODB9.MQye4OsqFKnDZArhNpS8uhSPaAF7NfjXFevZFPolPvUWfzYNLvhHOwX05QeA2jJfL12QWpam7nGeSxb1nIdnK1Qas95_ep04YLk1wS7M0OE9wdnvopJHaHanvDWttnybA12dCfA79vwU6bD0IwVI7Hjsm3740Y_BzlyAfX8Lye8dWI3-slcFa8_XNt16sZ3FA9oDI6T99tHkoMJeuAvHs4kQhv5UIqUWfbbxbSw_gTNM3AaeQ5vLrTlqjU6TQrogQjPzEuz83zOX-X-xUrvHloYd7Pwn2XDAWHWhHHLuXtsCbfnGestXkrMBdscNne5jlNPU16_GxfLglWdq1Mllhg")
      decodeAndCheckMetadata(authService, metadata)
    }

    "parse and configure the authorisation mechanism correctly when `--auth-jwt-es256-crt <PK.crt>` is passed" in {
      val tmpCrtFile = getTmpCrtFileAbsolutePath("/test.pubk.ecdsa256.crt")
      val config = Cli.parse(Array("--auth-jwt-es256-crt", tmpCrtFile))
      config shouldBe defined
      val authService = getAuthService(config)
      val metadata = getAuthMetadata("eyJhbGciOiJFUzI1NiIsInR5cCI6IkpXVCJ9.eyJodHRwczovL2RhbWwuY29tL2xlZGdlci1hcGkiOnt9LCJleHAiOjE5MDA4MTkzODB9.mLwYV8-2nNEXUcwhp4m3HpIkyomJRPLwJc1hH8g56UYfjxACqXHTQngt-2mvWwPPyvrVZXd7nU3Q-JdxFrPBYA")
      decodeAndCheckMetadata(authService, metadata)
    }

    "parse and configure the authorisation mechanism correctly when `--auth-jwt-es512-crt <PK.crt>` is passed" in {
      val tmpCrtFile = getTmpCrtFileAbsolutePath("/test.pubk.ecdsa512.crt")
      val config = Cli.parse(Array("--auth-jwt-es512-crt", tmpCrtFile))
      config shouldBe defined
      val authService = getAuthService(config)
      val metadata = getAuthMetadata("eyJhbGciOiJFUzUxMiIsInR5cCI6IkpXVCJ9.eyJodHRwczovL2RhbWwuY29tL2xlZGdlci1hcGkiOnt9LCJleHAiOjE5MDA4MTkzODB9.AXL5BqqeEfiu-kkY_A3mBLHAa2rQtHeygITSSfhUYubcooJlvtknZtkYlTkqA-IyKGRkly29LCk395BkTXug3vbgAeTEv7DB9mXDrCU1I1Z5YTrs64lXVKii58jqhWWWSezIAZkWhkv5aHZHyy_Y7DJFecWME2qhSfBAdoPlFMs0O4FO")
      decodeAndCheckMetadata(authService, metadata)
    }

    "parse and configure the authorisation mechanism correctly when `--auth-jwt-rs256-jwks <URL>` is passed" in {
      // Generate some RSA key pairs
      val keySize = 2048
      val kpg = KeyPairGenerator.getInstance("RSA")
      kpg.initialize(keySize)

      val keyPair = kpg.generateKeyPair()
      val publicKey = keyPair.getPublic.asInstanceOf[RSAPublicKey]
      val privateKey = keyPair.getPrivate.asInstanceOf[RSAPrivateKey]

      val token = generateToken("test-key-1", privateKey)
        .fold(e => fail("Failed to generate signed token: " + e.shows), identity)

      // Start a JWKS server and create a verifier using the JWKS server
      val jwks = KeyUtils.generateJwks(
        Map(
          "test-key-1" -> publicKey
        ))

      val server = SimpleHttpServer.start(jwks)

      val url = SimpleHttpServer.responseUrl(server)
      val config = Cli.parse(Array("--auth-jwt-rs256-jwks", url))
      config shouldBe defined
      val authService = getAuthService(config)
      val metadata = getAuthMetadata(token.value)
      val assertion = decodeAndCheckMetadata(authService, metadata)
      SimpleHttpServer.stop(server)
      assertion
    }
  }

  private[this] def getAuthService(config: Option[Config]) = {
    val kvUtilsAppConfig = KVBCLedgerFactory.toKVUtilsAppConfig(config.get)
    val authService = KVBCLedgerFactory.authService(kvUtilsAppConfig)
    authService
  }

  private[this] def getAuthMetadata(token: String) = {
    val metadata = new Metadata()
    metadata.put(AuthServiceJWT.AUTHORIZATION_KEY, s"Bearer $token")
    metadata
  }

  private[this] def decodeAndCheckMetadata(authService: AuthService, metadata: Metadata): Future[Assertion] =
    authService.decodeMetadata(metadata).toScala.map(_.claims should be(List(ClaimPublic)))

  private[this] def getTmpCrtFileAbsolutePath(resourcePath: String): String = {
    val resourceURL = getClass.getResource(resourcePath)
    val tmpFile = {
      val f = File.createTempFile("vdaml-api-server-test", ".pk.crt")
      f.deleteOnExit()
      f.getAbsolutePath
    }
    Files.copy(resourceURL.openStream(), Paths.get(tmpFile), StandardCopyOption.REPLACE_EXISTING)
    tmpFile
  }

  private[this] def generateToken(keyId: String, privateKey: RSAPrivateKey): JwtSigner.Error \/ domain.Jwt = {
    val jwtPayload = s"""{"test": "JwksSpec"}"""
    val jwtHeader = s"""{"alg": "RS256", "typ": "JWT", "kid": "$keyId"}"""
    JwtSigner.RSA256.sign(DecodedJwt(jwtHeader, jwtPayload), privateKey)
  }
}

