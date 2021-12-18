package com.pako.wav

import monocle.macros.GenLens
import monocle.Lens
import cats.effect.IO
import cats.implicits._
import cats.Applicative


case class WavHeader(
    riff : Seq[Byte],
    size : Seq[Byte],
    descrip : Seq[Byte],
    fmt : Seq[Byte],
    fmtSize : Seq[Byte],
    format : Seq[Byte],
    channels : Seq[Byte],
    sampleRate : Seq[Byte],
    byteRate : Seq[Byte],
    blockAlign : Seq[Byte],
    bitsPerSample : Seq[Byte],
    dataDescription : Seq[Byte],
    dataSize : Seq[Byte],
    data: Seq[Byte]
)



object Reader {
    def read[F[_]](
        size: Int,
        lens: Lens[WavHeader, Seq[Byte]]): F[Seq[Byte]] = ???
}

object Parser {

    import Reader._

    def wavHeaderParser[F[_]]()(implicit F: Applicative[F]): F[WavHeader] = 
    (
        read(4, GenLens[WavHeader](_.riff)),
        read(4, GenLens[WavHeader](_.size)),
        read(4, GenLens[WavHeader](_.descrip)),
        read(4, GenLens[WavHeader](_.fmt)),
        read(4, GenLens[WavHeader](_.fmtSize)),
        read(2, GenLens[WavHeader](_.format)),
        read(2, GenLens[WavHeader](_.channels)),
        read(4, GenLens[WavHeader](_.sampleRate)),
        read(4, GenLens[WavHeader](_.byteRate)),
        read(2, GenLens[WavHeader](_.blockAlign)),
        read(2, GenLens[WavHeader](_.bitsPerSample)),
        read(4, GenLens[WavHeader](_.dataDescription)),
        read(4, GenLens[WavHeader](_.dataSize)),
        F.pure(Seq[Byte]())
    ).mapN(WavHeader)
}