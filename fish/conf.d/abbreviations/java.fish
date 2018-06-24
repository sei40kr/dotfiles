# java.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

# mvn

abbr -a mvncie mvn clean install eclipse:eclipse
abbr -a mvnci mvn clean install
abbr -a mvncist mvn clean install -DskipTests
abbr -a mvncisto mvn clean install -DskipTests --offline
abbr -a mvne mvn eclipse:eclipse
abbr -a mvnce mvn clean eclipse:clean eclipse:eclipse
abbr -a mvncv mvn clean verify
abbr -a mvnd mvn deploy
abbr -a mvnp mvn package
abbr -a mvnc mvn clean
abbr -a mvncom mvn compile
abbr -a mvnct mvn clean test
abbr -a mvnt mvn test
abbr -a mvnag mvn archetype:generate
abbr -a mvn-updates mvn versions:display-dependency-updates
abbr -a mvntc7 mvn tomcat7:run
abbr -a mvntc mvn tomcat:run
abbr -a mvnjetty mvn jetty:run
abbr -a mvndt mvn dependency:tree
abbr -a mvns mvn site
abbr -a mvnsrc mvn dependency:sources
abbr -a mvndocs mvn dependency:resolve -Dclassifier=javadoc
