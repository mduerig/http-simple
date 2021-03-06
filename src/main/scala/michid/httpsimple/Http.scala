package michid.httpsimple

import org.apache.http.entity.FileEntity
import java.io.File
import org.apache.http.impl.client.{BasicCredentialsProvider, DefaultHttpClient}
import org.apache.http.auth.{UsernamePasswordCredentials, AuthScope}
import org.apache.http.client.protocol.ClientContext
import org.apache.http.client.params.ClientPNames
import java.net.URI
import org.apache.http.protocol.BasicHttpContext
import org.apache.http.{HttpResponse, HttpEntity}
import org.apache.http.message.BasicNameValuePair
import org.apache.http.client.entity.UrlEncodedFormEntity
import org.apache.http.client.methods.{HttpDelete, HttpPut, HttpPost, HttpGet}
import collection.JavaConversions._

object Http {
  val httpClient = new DefaultHttpClient()

  def apply[T](request: Request, responseHandler: HttpResponse => T): T = request match {
    case Get(credentials, uri) =>
      handleResponse(get(credentials, uri), responseHandler)

    case Post(credentials, uri, entity) =>
      handleResponse(post(credentials, uri, entity), responseHandler)

    case Put(credentials, uri, entity) =>
      handleResponse(put(credentials, uri, entity), responseHandler)

    case Delete(credentials, uri) =>
      handleResponse(delete(credentials, uri), responseHandler)

    case _ => sys.error("Unknown request: " + request)
  }

  private def get(credentials: Option[(String, String)], uri: URI) = {
    httpClient.execute(new HttpGet(uri), buildContext(credentials).getOrElse(null))
  }

  private def post(credentials: Option[(String, String)], uri: URI, entity: HttpEntity) = {
    val post = new HttpPost(uri)
    post.setEntity(entity)
    httpClient.execute(post, buildContext(credentials).getOrElse(null))
  }

  private def put(credentials: Option[(String, String)], uri: URI, entity: HttpEntity) = {
    val put = new HttpPut(uri)
    put.setEntity(entity)
    httpClient.execute(put, buildContext(credentials).getOrElse(null))
  }
  
  private def delete(credentials: Option[(String, String)], uri: URI) = {
    httpClient.execute(new HttpDelete(uri), buildContext(credentials).getOrElse(null))
  }

  private def buildContext(credentials: Option[(String, String)]) = credentials.map {
    case (user, pass) => {
      val credentialsProvider = new BasicCredentialsProvider();
      credentialsProvider.setCredentials(AuthScope.ANY, new UsernamePasswordCredentials(user, pass))

      val httpContext = new BasicHttpContext();
      httpContext.setAttribute(ClientContext.CREDS_PROVIDER, credentialsProvider)
      httpContext.setAttribute(ClientPNames.HANDLE_AUTHENTICATION, true)
      httpContext
    }
  }

  private def handleResponse[T](response: HttpResponse, responseHandler: HttpResponse => T) = {
    try {
      responseHandler(response)
    }
    finally {
      Option(response.getEntity).foreach(_.getContent.close())
    }
  }
}

sealed trait Request
case class Get(credential: Option[(String, String)], uri: URI) extends Request
case class Post(credential: Option[(String, String)], uri: URI, entity: HttpEntity) extends Request
case class Put(credential: Option[(String, String)], uri: URI, entity: HttpEntity) extends Request
case class Delete(credential: Option[(String, String)], uri: URI) extends Request

object Response {
  def unapply(httpResponse: HttpResponse): Option[(Int, String, HttpResponse)] = {
    val status = httpResponse.getStatusLine
    Some((status.getStatusCode, status.getReasonPhrase, httpResponse))
  }
}

object Get {
  def apply(uri: String): Request =
    Get(None, URI.create(uri))

  def apply(credentials: (String, String), uri: String): Request =
    Get(Some(credentials), URI.create(uri))
}

object Post {
  def apply(uri: String, entity: HttpEntity): Request = {
    Post(None, URI.create(uri), entity)
  }

  def apply(credentials: (String, String), uri: String, entity: HttpEntity): Request = {
    Post(Some(credentials), URI.create(uri), entity)
  }
}

object Put {
  def apply(uri: String, entity: HttpEntity): Request =
    Put(None, URI.create(uri), entity)

  def apply(credentials: (String, String), uri: String, entity: HttpEntity): Request =
    Put(Some(credentials), URI.create(uri), entity)
}

object Delete {
  def apply(uri: String): Request =
    Delete(None, URI.create(uri))

  def apply(credentials: (String, String), uri: String): Request =
    Delete(Some(credentials), URI.create(uri))
}

object Entity {
  def apply(file: File, contentType: String) = new FileEntity(file, contentType)

  def apply(values: List[(String, String)]) = {
    val params = values.map {
      case (name, value) => new BasicNameValuePair(name, value)
    }

    new UrlEncodedFormEntity(params, "UTF-8")
  }
}

