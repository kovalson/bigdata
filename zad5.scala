import scala.io.Source
import java.io._

var StopRaw = "go, a, aby, ach, acz, aczkolwiek, aj, albo, ale, ależ, ani, aż, bardziej, bardzo, bo, bowiem, by, byli, bynajmniej, być, był, była, było, były, będzie, będą, cali, cała, cały, ci, cię, ciebie, co, cokolwiek, coś, czasami, czasem, czemu, czy, czyli, daleko, dla, dlaczego, dlatego, do, dobrze, dokąd, dość, dużo, dwa, dwaj, dwie, dwoje, dziś, dzisiaj, gdy, gdyby, gdyż, gdzie, gdziekolwiek, gdzieś, i, ich, ile, im, inna, inne, inny, innych, iż, ja, ją, jak, jaka, jakaś, jakby, jaki, jakichś, jakie, jakiś, jakiż, jakkolwiek, jako, jakoś, je, jeden, jedna, jedno, jednak, jednakże, jego, jej, jemu, jest, jestem, jeszcze, jeśli, jeżeli, już, ją, każdy, kiedy, kilka, kimś, kto, ktokolwiek, ktoś, która, które, którego, której, który, których, którym, którzy, ku, lat, lecz, lub, ma, mają, mało, mam, mi, mimo, między, mną, mnie, mogą, moi, moim, moja, moje, może, możliwe, można, mój, mu, musi, my, na, nad, nam, nami, nas, nasi, nasz, nasza, nasze, naszego, naszych, natomiast, natychmiast, nawet, nią, nic, nich, nie, niech, niego, niej, niemu, nigdy, nim, nimi, niż, no, o, obok, od, około, on, ona, one, oni, ono, oraz, oto, owszem, pan, pana, pani, po, pod, podczas, pomimo, ponad, ponieważ, powinien, powinna, powinni, powinno, poza, prawie, przecież, przed, przede, przedtem, przez, przy, roku, również, sama, są, się, skąd, sobie, sobą, sposób, swoje, ta, tak, taka, taki, takie, także, tam, te, tego, tej, temu, ten, teraz, też, to, tobą, tobie, toteż, trzeba, tu, tutaj, twoi, twoim, twoja, twoje, twym, twój, ty, tych, tylko, tym, u, w, wam, wami, was, wasz, wasza, wasze, we, według, wiele, wielu, więc, więcej, wszyscy, wszystkich, wszystkie, wszystkim, wszystko, wtedy, wy, właśnie, z, za, zapewne, zawsze, ze, zł, znowu, znów, został, żaden, żadna, żadne, żadnych, że, żeby, -"
val Stop = StopRaw.split(", ").toSeq
val pw = new PrintWriter( new File("output.txt" ), "UTF-8" )
val SOURCE_FILE = "./ostatnie_zyczenie.txt"

var content = Source
		.fromFile( SOURCE_FILE, "UTF-8" )
		.mkString
		.toLowerCase
		.replaceAll( "[.,-?!]", "" )
		.split( "\\s+" )
		.filterNot( Stop.contains( _ ) )
		.map( i => (i, 1) )
		.groupBy( x => x._1 )
		.mapValues( x => x.length )
		.toSeq
		.sortWith( (x, y) => x._2 > y._2 )
		.slice( 0, 200 )

pw.write( content.map( i => i._2 + "\t" + i._1 + "\n" ).toString )
pw.close

// pw.write( content )
// pw.close

// println( Source.fromFile( SOURCE_FILE, "UTF-8" ).mkString.toLowerCase )
// pw.write( Source.fromFile( "./ostatnie_zyczenie.txt", "UTF-8" ).mkString.toLowerCase.replaceAll("\\.","").replaceAll(",","").split("\\s+").filterNot( Stop.contains(_) ).map( i => (i, 1) ).groupBy( x => x._1 ).mapValues( x => x.length ).toSeq.sortWith( (x, y) => x._2 > y._2 ).toString )
// pw.close