module Day6 (day6) where

import Data.List
import Data.List.Split
import Data.Maybe

input :: String
input = "llqnqffqsqttfffbcfcbcbdcczccfssvwswrwddzlddpdhdwwlvlffjllnjjwjqwjjttwbwcwfccdmmnddgvvpwvvgsshnshsgglljfjzjpjfpfjpplddjcchdhvhlhvllvflfbllsdllgppwjjprjpjrrdwrdrggjvjppgbgttdppwhhcshsvvgpvggsllstsggdjdmjjrvjjszjsjbbsffjwjnwwzjjjvqvfftbffbpffndfdzfdfvdfdggmpmbbwgbgnnbtnnnhggdmdffrqrlrhrzzrmzzmbzzcdcwwzffsrrnfnvfnnvppwjjndjnndtdppgcppsmppljlpjjmlldlsltlglwgwcwnwvwddzrrllwjjnvjvwvppjssncnfcnfcfcczfccpjphjphjjjsgszzhthghjhrjrbrtrjrhrsrfftfzftfmmwmpmgghbggjrrsdswddtjjvnnrwrzrpzzlglwggrnrgrfftnffwwgllrqqzbqbbtltbbgdgpgphggspggplggmcmscsffzcfzzbggdrgrqgrrnlrnrbnnzsnnzcctvvnvwvnwnhhwpwtptllpflfcfttwtjjhwjhhbwhbbtppwhwvhvghvhphpwwcgwwhbbfvbffzpzlllrzlrrbnnrngrnrpnnsszbbqffpsffhfshfhzzqhhcgcgfggzmmdllthhrhnrrwggdqdsstccqllflmflfddjwjzjffvjjfgjgdgbdgdngnpgpnpffsnsjnnbbjdbjbtbmmbrrlbbqmqpqrprjjrbbvnbbzvvcwwlfwfggmhhdhsdhsdhshhqfhfrhhqlqttffpmmjzjqjggqzzdfzflfsllshhvjvfvbfvbbjljhhzrzqqszqzsqqswswbsbzszgzdgzzhjzhhvffhthvtthltthghzhvvjttczttlssvvgjjmsjstjjrfjjhbjbnjbjddqrddnbdnbnwbnbqbmqqgtgqtttcmmqbqrrgrrsrszssvpsvvjqjttjpjwwmwfwttczttgccwhcwwrzwzbwwqbqmqnmqqnfnmmmzdmzmpmssdpsslbbmgmbmlmnlldlccvzzlrzzqbqfqlflwlvlhhtrtcttgnnqhnqqtjjphjhwjhwhpwwvdvfddmndncnppcffhllfvfdfllhgslvtsqhtlfdflcjfmqbnctnfnwqrlqbzrcbvldrffcptsgslqcszqcfdvtpggvdqblwcgmdjqrpjdhtrmvrfrzznspqlfhnjsppbpjdggcwjwprpnlnntgfgmflctqphdmzfvpzzmbzmvrqdgchzmdvjdzmfsslpqvhpgznmpspjpdmlfwwjbbwqbfthghclldpmnsbcwlzswrsnfzbdzpcnrrpspdpfqhvmtfjlppqtphvzzqrwhzccnrgrtgfbfgtwvlwsmcvzmqmhsvztmmvpjzfwzgfwntbrsfthdgrcmgtdsvzcllmcshrlqldrvrnmdgbwttmhczvscrdvfgdvrhfvlghhsfbmrptbwmpnvtsrjlpjlbmmjzwwzbdtjlqqdczqgpzfjslccrcrblhplndblghchczbjjfzlsvvrqhvgdsncgpjhjlprhfhswwbmrnszqzhhlrbqpphvgtfsgmdpjwgcmqnvfdhrqmbspjpdrtdbqnbmbpgqwgmltqwrjprvsfjsmpldcqqbvmfhgzltzfvhlnfdqrphzzjrbdvnnjspvnlnnsdzvgqsqztndjpmnbqtwnpzmmfhsswwnnwwlbnpgbrhzchbnsrwwpprhntngsjzvssttqwfvjrdddtfpgtqqzcwljzmdjtgzdqjjvbqgdttdgvqvlfdsgcjhsmdmwrwdcqdflpfjbfzsvjrzrhhcnvcjblwcdvtbgfhfgcwrcjsrzcdrfwtvdqrghdtrjgdmhrfcsnwwwdpvjtpzdqfgrlmrqscjbfgdbgvflhvdjmnmslvsbcbgwplgqljmlzpgrfjwmvqfwmwrhnmdjhdwgjrngvccrbzmhcqthvvtdtmfqvfczhqbfgzgrmdtprznfzjtrcwqgztchtdmzmnwbfbnbttbvzsflcpsjshgphfdlvhdrcpsqnhjjggbnsqrfpwsdznzcwjbcswwndzbpdnfcbdrfgrmqzvtjttltbntznmqfsmqlgqvlqnrvgrnggslqhbplmgpzwlfzbvwdvrchsnhrnvgmzjdprvvspltcdzmdnlgtmrwnwpdndpdqjltcnmsggrvbprslqhfgmzqtppdpsjcmmbvfgmbpdnwdcgnssfgjhzhrjljdwhrzznscndgbscdmbbtbrnzbqzvcjgjgljbjlrrvdhjdllsnjzhwlmjslghrqplwjwssbzzpdzdfhhsqctlcddnfnnvbcwpdvzdcsgcqpctsjtdtnzpggpzsrrhfjtthqcqhtvwzltbdvdnbgwlppblwzjsqqbcpcrthhrhdnzhdnflqlvbzmcjfcrbmgdgqptfqfbmlfbblqdfmnwgvbdhmcmtmvtggqstjpwhvzjhbgpblmdrnggvrvphbglqgfcphmrgfmrwcdchtwfllqwsnbqttwdcvrwgzjfztmcffppqtmnwpgcrgwtjbdtjlmnpmvlzndljglzblwdrggqvbbfvqcbcbpqttrmqlcqnqvrfqsnlpmwlcgfwfcqpgmszfccbqtcqfwlwqrjjhrdbjqvdmfzjgncjqgqbthpgjgbfdvltbhpnbjqqwrsczrthfhmlzjjjgsjtsvgmwfsjngzfqdqzfhvwjrswvnqvsvvsjdbwlwdcsszdngmmhnnqsgvsrvpnndghrwgzztqczvhcrzdpqtrmrnfsfrlpdnbbtshfhplzqvdvzdvwhwsbpnbzlvcbgptdszjlcgfdzchjcsvhzdljvgpwstzwnssvhztcptnhslggnrschvfnmhcnjvldthtfpqzdvltfgnmtgvlrljhwqdzqfmfblstvfnpfcdsqslrqbztrbfzmsfjtjwhlzfnhrvpfqfqvtdllrvchmqphgljwcspgpwsdwqfdhsqhsflpbcbjjmjrfjrqrqfqcqzqsqcnqhfgsclfnfzblfdhphrvqdpvcqmllrcdnrlwqbrgqsbfqqllcvmglntjwcsjljgntmmldscndfdjcqpwbqpbmfjsgwfwcqbqbbhhgprlbzmvdfjcsmsqvhfhmgrhnwpslztmwbhdgrfzfcmwjswpbpzwstfbfmgwtprmptzjwtrqthrqwgslnmtlfgnvgpwvsfwthtrgwfbnnnwmdcfrpqqztplscvfnfpfwwdnfnzjccnhswwlcrrdqfhvsrnvcdrwmjswzggscplggbwgndsbntqvtrjbmbzrnbbmdjvwrmmtrmfjjhnvrcjcbqlhlthbvtjjczddblbbttmmzgdqmtdqswjdwbjhsrjbvdtqzqdbhhgbttgmgwfgfpczpqpfsddgslltwsvngwbwfbfcdzlqghwdbfzzldjpwpmpjmslwnwbrjjvwcsjgdzjwrrwnvgvrqlgjhwvrgnczspfplhfbtdpbpfqmhbvmcqdgrrjfslzgsqfpwrrrmjdtgbslddwvddrbmrdsdhhnlwsncrmnglrrpvtbrfvjbdmcpgphcdfwnfcglvmlbslttpmjnspqhnmbcqgmncfjjpdfjqhggnswbgppjhllscrvtmtmmbwbpgddtzblscntrmccdpzdnllqpvfdpfpwwvnnbjlzphvqwffwsjmbtllctrjmllwscmldcdrpfrzrqlpwbjwfgmnshzqzgdjqhcwtsqlsjffvzcpnrzmvtlzlgwvrrjtdbcnddbhjgqqzrvhplrbsrwgscjnfmhbcnpdcjqrltgdzzzzbqtsspbcdssbjrzfqdgvhmgdzsjdsqcfwbgrnhrlzgpjmhctqdccmvqzddmcptsjgtfshprqmslvtmtrprfsngrnnpnrccrvnrvcwzrbbnbghlwvcncgzglnqthchhsnzlfrcggdptvwlrbnfwgjpflgrcfzhhgffwcbhwlsdmvmsvvzvdcrlvlnstgz"

day6 :: IO ()
day6 = do
  putStrLn $ ("day6a: " ++) $ show $ (+ 4) $ fromJust $ findIndex ((== 4) . length . nub) $ divvy 4 1 input
  putStrLn $ ("day6b: " ++) $ show $ (+ 14) $ fromJust $ findIndex ((== 14) . length . nub) $ divvy 14 1 input
