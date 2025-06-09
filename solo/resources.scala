package hrf

import hrf.colmat._

import scalajs.js

import org.scalajs.dom

package object html {

    case class Resources(images : ImageResources, names : () => Map[Any, String]) {
        def getName(o : Any) : Option[String] = names().get(o)
    }

    case class ImageResources(loaded : Map[String, hrf.loader.ImageWrapper], sources : Map[String, String], loader : hrf.loader.Loader[hrf.loader.ImageWrapper]) {
        var incomplete : $[String] = $

        def has(key : String) = loaded.contains(key.toLowerCase)
        def get(key : String) : dom.html.Image = {
            val img = loaded.get(key.toLowerCase).|(throw new Error("asset " + key + " not found")).get

            if (img.complete.not)
                incomplete :+= key

            img
        }
        def hasSource(key : String) = sources.contains(key.toLowerCase)
        def getSource(key : String) : String = loaded.get(key.toLowerCase)./(_.get.src).||(sources.get(key.toLowerCase)).|(throw new Error("asset " + key + " not found"))
        def getBlobSource(key : String)(onLoad : String => Unit) {
            loaded.get(key.toLowerCase)./(_.get.src) @@ {
                case Some(url) => onLoad(url)
                case None => sources.get(key.toLowerCase) @@ {
                    case Some(url) => loader.wait($(url)) { onLoad(loader.get(url).get.src) }
                    case None => throw new Error("asset " + key + " not found")
                }
            }
        }
    }
}
