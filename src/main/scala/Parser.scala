package com.pako.wav

import java.io.InputStream
import java.nio.ByteBuffer

import cats._
import cats.data._
import cats.implicits._


object HeaderProtocolEntry {
    private def composeParserFunc[CLS, T, S](wav1: HeaderProtocolEntry[CLS, T], wav2: HeaderProtocolEntry[CLS, S], cls: CLS, arr: Array[Byte]): (T, S) = {
        val arrSplit = arr.splitAt(wav1.toRead(cls))
        (wav1.parse(cls, arrSplit._1), wav2.parse(cls, arrSplit._2))
    }

    private def compLens[CLS, T, S](lens1: (CLS, T) => CLS, lens2: (CLS, S) => CLS, cls: CLS, ts: (T, S)): CLS =
        lens2(lens1(cls, ts._1), ts._2)


    def compose[CLS, T, S](wav1: HeaderProtocolEntry[CLS, T], wav2: HeaderProtocolEntry[CLS, S]): HeaderProtocolEntry[CLS, (T, S)] =
        HeaderProtocolEntryFunc[CLS, (T, S)](
                in => wav1.toRead(in) + wav2.toRead(in),
                (cls: CLS, arr: Array[Byte]) => composeParserFunc(wav1, wav2, cls, arr),
                (cls: CLS, tuple: Tuple2[T, S]) => compLens(wav1.copy, wav2.copy, cls, tuple))

    def composeAppl[CLS, RES, T, S]
                    (wav1: HeaderProtocolEntry[CLS, T], wav2: HeaderProtocolEntry[CLS, S])
                    (func: Tuple2[T, S] => RES, invFunc: RES => Tuple2[T, S]): HeaderProtocolEntry[CLS, RES] = {
        val comp = compose(wav1, wav2)
        HeaderProtocolEntryFunc[CLS, RES](
                comp.toRead,
                (cls: CLS, arr: Array[Byte]) => func(comp.parse(cls, arr)),
                (cls: CLS, res: RES) => {
                    val res2 = comp.copy(cls, invFunc(res))
                    println("res!!! " + res2)
                    res2
                }
        )
    }
}

trait HeaderProtocolEntry[CLS, T] {
    def toRead(cls: CLS): Int
    def parse(cls: CLS, bytes: Array[Byte]): T
    def copy(cls: CLS, t: T): CLS

    def andThen[S](other: HeaderProtocolEntry[CLS, S]): HeaderProtocolEntry[CLS, (T, S)] = HeaderProtocolEntry.compose(this, other)

    def parseAndCopy(in: CLS, read: Int => Array[Byte]) : CLS =
        copy(in, parse(in, read(toRead(in))))
}

case class HeaderProtocolEntryFunc[CLS, T](_toRead: CLS => Int, _parse: (CLS, Array[Byte]) => T, _copy: (CLS, T) => CLS) extends HeaderProtocolEntry[CLS, T] {
    def toRead(cls: CLS): Int = _toRead(cls)
    def parse(cls: CLS, bytes: Array[Byte]): T = _parse(cls, bytes)
    def copy(cls: CLS, t: T): CLS = _copy(cls, t)
}

case class HeaderProtocolEntryFixed[CLS, T](_toRead: Int, _parse: Array[Byte] => T, _copy: (CLS, T) => CLS) extends HeaderProtocolEntry[CLS, T] {
    def toRead(cls: CLS): Int = _toRead
    def parse(cls: CLS, bytes: Array[Byte]): T = _parse(bytes)
    def copy(cls: CLS, t: T): CLS = _copy(cls, t)


}

class FmtExtraParamParserSize extends HeaderProtocolEntry[WavHeader, Option[Int]] {
    def toRead(cls: WavHeader): Int = if(cls.audioFormat == WavHeader.PCM) 0 else 2
    def parse(cls: WavHeader, bytes: Array[Byte]): Option[Int] = {
        println("parse to read " + bytes.size + " " + cls.audioFormat)
        if(cls.audioFormat != WavHeader.PCM) Some(Parser.toInt(bytes)) else None;
    }
    def copy(cls: WavHeader, t: Option[Int]): WavHeader = cls.copy(extraParam = 
        cls.extraParam.map(ep => ep.copy(size = t.getOrElse(-1)))
    ) 
}

class FmtExtraParamParserData extends HeaderProtocolEntry[WavHeader, Option[Array[Byte]]] {
    def toRead(cls: WavHeader): Int = cls.extraParam match {
            case Some(ep) => ep.size
            case None => 0
        }

    def parse(cls: WavHeader, bytes: Array[Byte]): Option[Array[Byte]] =
        cls.extraParam match {
            case Some(ep) => Some(bytes)
            case None => None
        }
    def copy(cls: WavHeader, t: Option[Array[Byte]]): WavHeader = cls.copy(extraParam = 
        cls.extraParam match {
            case Some(ep) => Some(ep.copy(extraParam = t.get))
            case None => None
        }
    ) 
}




object Parser {

    private val waveHeaderProtocol = (
        HeaderProtocolEntryFixed[WavHeader, String](4, toStr, (wh, in) => wh.copy(riff = in)) andThen
        HeaderProtocolEntryFixed[WavHeader, Int](4, toInt, (wh, in) => wh.copy(size = in)) andThen
        HeaderProtocolEntryFixed[WavHeader, String](4, toStr, (wh, in) => wh.copy(wave = in)) andThen
        HeaderProtocolEntryFixed[WavHeader, String](4, toStr, (wh, in) => wh.copy(fmt = in.trim())) andThen
        HeaderProtocolEntryFixed[WavHeader, Int](4, toInt, (wh, in) => wh.copy(fmtSize = in)) andThen
        HeaderProtocolEntryFixed[WavHeader, Int](2, toInt, (wh, in) => wh.copy(audioFormat = in)) andThen
        HeaderProtocolEntryFixed[WavHeader, Int](2, toInt, (wh, in) => wh.copy(numChannels = in)) andThen
        HeaderProtocolEntryFixed[WavHeader, Int](4, toInt, (wh, in) => wh.copy(sampleRate = in)) andThen
        HeaderProtocolEntryFixed[WavHeader, Int](4, toInt, (wh, in) => wh.copy(byteRate = in)) andThen
        HeaderProtocolEntryFixed[WavHeader, Int](2, toInt, (wh, in) => wh.copy(blockAlign = in)) andThen
        HeaderProtocolEntryFixed[WavHeader, Int](2, toInt, (wh, in) => wh.copy(bitsPerSample = in)) andThen
        HeaderProtocolEntry.composeAppl[WavHeader, Option[FmtExtraParam], Option[Int], Option[Array[Byte]]]
                (new FmtExtraParamParserSize, new FmtExtraParamParserData)
                    (opIntArr => (opIntArr._1, opIntArr._2).mapN(FmtExtraParam),opFmt => Tuple2(opFmt.map(_.size), opFmt.map(_.extraParam)))
    )

    def wavHeaderParser(is: InputStream) : WavHeader = {
        val res = WavHeader.empty()
        val res3 = waveHeaderProtocol.parseAndCopy(res, is.readNBytes)
        res3
    }

    def toStr(arr: Array[Byte]): String = return new String(arr)

    private def toHex(bytes: Array[Byte]): String = {
        val result = new StringBuilder();
        for (temp <- bytes) {
            result.append(String.format("%02x", temp));
        }
        result.toString();
    }


    def toInt(arr: Array[Byte]): Int = {
        val bArr: Array[Byte] = Array(0x00, 0x00, 0x00, 0x00)
        Array.copy(arr, 0, bArr, 0, arr.size)
        //println(toHex(arr) + " -> " + toHex(bArr))
        ByteBuffer.wrap(bArr).order(java.nio.ByteOrder.LITTLE_ENDIAN).getInt()
    }


}