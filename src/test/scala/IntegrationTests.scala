import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers._

import net.ironforged.scaladiff._

class IntegrationSuite extends FunSuite {
  test("simple sentence modification produces the expected diff") {
    val original = "bills boards"
    val modified = "bills swords"
    val diff = Diff.create(original, modified)
    diff.toString should equal("bills +s+w-bo-ards")
    diff.humanized should equal("bills -[boa]+[swo]rds")
    diff.html should equal("<span>bills </span><del>boa</del><ins>swo</ins><span>rds</span>")
  }

  test("modified lorem ipsum produces the expected diff") {
    val originalloremIpsum =
      """
        |Lorem ipsum dolor sit amet, consectetur adipiscing elit.
        |Donec rhoncus congue risus ac viverra.
        |Praesent varius orci quis turpis vestibulum, eget molestie augue aliquet.
        |Integer feugiat felis molestie sodales scelerisque.""".stripMargin
    val modifiedLoremIpsum =
      """
        |Lorem ipsum amet sit dolor, consectetur adipiscing elit.
        |Praesent varius orci quis turpis vestibulum, eget molestie augue aliquet.
        |Donec rhoncus congue risus ac viverra.
        |Integer feugiat felis molestie sodales scelerisque.""".stripMargin

    val diff = Diff.create(originalloremIpsum, modifiedLoremIpsum)

    diff.toString should equal(
      """
        |Lorem ipsum +a+m+e+t+ +s+i+t+ dolor- -s-i-t- -a-m-e-t, consectetur adipiscing elit.
        |-D-o-n-e-c- -r-h-o-n-c-u-s- -c-o-n-g-u-e- -r-i-s-u-s- -a-c- -v-i-v-e-r-r-a-.-
        |Praesent varius orci quis turpis vestibulum, eget molestie augue aliquet.
        |+D+o+n+e+c+ +r+h+o+n+c+u+s+ +c+o+n+g+u+e+ +r+i+s+u+s+ +a+c+ +v+i+v+e+r+r+a+.+
        |Integer feugiat felis molestie sodales scelerisque.""".stripMargin)

    diff.humanized should equal(
      """
        |Lorem ipsum +[amet sit ]dolor-[ sit amet], consectetur adipiscing elit.
        |-[Donec rhoncus congue risus ac viverra.
        |]Praesent varius orci quis turpis vestibulum, eget molestie augue aliquet.
        |+[Donec rhoncus congue risus ac viverra.
        |]Integer feugiat felis molestie sodales scelerisque.""".stripMargin)

    diff.html should equal("<span>&para;<br>Lorem ipsum </span><ins>amet sit </ins><span>dolor</span><del> sit amet</del><span>, consectetur adipiscing elit.&para;<br></span><del>Donec rhoncus congue risus ac viverra.&para;<br></del><span>Praesent varius orci quis turpis vestibulum, eget molestie augue aliquet.&para;<br></span><ins>Donec rhoncus congue risus ac viverra.&para;<br></ins><span>Integer feugiat felis molestie sodales scelerisque.</span>")
  }
}