package hrf

import hrf.colmat._

import hrf.web._

import hrf.loader._

import org.scalajs.dom

import scalajs.js.timers.setTimeout
import scalajs.js.Dynamic

import scala.collection.mutable


object HRF {
    val imageDataVersion = "as-of-0.8.87"

    def now() = new scalajs.js.Date()

    val startAt = now()

    def uptime() : Int = (now().getTime() - startAt.getTime()).toInt

    val imageCache = new CachedBlobImageLoader("hrf-image-cache-" + imageDataVersion)
    val stringCache = new CachedStringLoader("hrf-page-cache")
    val stringLoader = StringLoader

    private val settings = getElem("settings").?

    def hash = dom.window.location.hash.drop(1)
    val search = dom.window.location.search.drop(1)

    private def cookieParam(p : String) = web.getCookie("hrf-param-" + p)
    private def settingsParam(p : String) = settings./~(_.getAttribute("data-" + p).?).but("")
    private def hashParam(p : String)  = hash.split('|').$./(_.split('=')).%(_(0) == p).single./(_.drop(1).join("=")).map(java.net.URLDecoder.decode(_, "UTF-8"))
    private def urlParam(p : String) = search.split('&').$./(_.split('=')).%(_(0) == p).single./(_.drop(1).join("=")).map(java.net.URLDecoder.decode(_, "UTF-8"))

    private val params = mutable.Map[String, |[String]]()

    dom.window.onhashchange = e => params.clear()

    if (cookieParam("cache-html").any)
        HRF.stringCache.queue(dom.window.location.origin + dom.window.location.pathname)

    def param(p : String) = params.getOrElseUpdate(p, hashParam(p) || urlParam(p) || cookieParam(p) || settingsParam(p))

    def flag(p : String) = param(p).but("-").but("false").but("no").any

    def paramInt(p : String) = param(p)./~(_.toIntOption)

    def paramList(p : String) = param(p)./~(_.split(' ').$)

    val embedded = flag("embedded-assets")

    val html = dom.window.location.origin // + "/play/"
    val script = dom.document.getElementById("script").asInstanceOf[dom.html.Script].src

    var originalOuterHtml : String = ""
}

