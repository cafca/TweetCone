# TweetCone

TweetCone ist eine Anwendung zur Visualisierung von Trending Twitter-Hashtags mithilfe der ConeCanvas Library.  Die Anwendung besteht aus zwei Komponenten: (1) Eine im Hintergrund laufende Anwendung, welche regelmäßig die Twitter API nach den aktuell populärsten Hashtags abfragt und diese lokal als JSON Serialisierung speichert und (2), einer auf der ConeCanvas library basierende Anwendung, welche die in (1) erfassten Daten visualisiert. 
TweetCone demonstriert die Basis-Funktionalität von ConeCanvas anhand von Tweets, eine Datenstruktur, die vielen Nutzern bekannt ist. Durch die Navigation im Baum kann der Nutzer erkunden, welche Hashtags momentan populär sind, wie sie mit anderen Hashtags kombiniert werden, und die zeitliche Entwicklung dieser Informationen.

## Beschreibung der Visualisierung (Version 1)

Die Visualisierung ist als ConeCanvas mit drei Ebenen realisiert.

	1. 	Knoten auf der obersten Ebene entsprechen den in der Vergangenheit erfassten Sets von Trending Hashtags. Jeder Knoten auf dieser Ebene repräsentiert einen Datensatz, welcher zu einem gegebenen Zeitpunkt erfasst wurde und ist mit einem Text-Label versehen, welches den Zeitpunkt der Erfassung kennzeichnet.
	2.	 Jeder solcher Datensatz enthält die zehn populärsten Twitter Hashtags zum gegebenen Zeitpunkt. Diese sind nach absteigender Popularität sortiert. Jeder Knoten entspricht einem einzelnen Hashtag und ist mit einem Text-Label versehen, welches dieses wiedergibt.
	3. 	Auf der untersten Ebene werden zu jedem Hashtag die zehn am stärksten mit diesem korrelierenden weiteren Hashtags gezeigt Da die Twitter API diese Information nicht direkt zur Verfügung stellt, werden für jedes Hashtag in der 2. Ebene die 1000 zuletzt hierzu verfassten Tweets akkumuliert und die in diesen vorkommenden weiteren Hashtags gezählt.

## Beschreibung der Visualisierung (Version 2)

Die Visualisierung ist als ConeCanvas mit zwei Ebenen realisiert. Zusätzlich wird ein Slider in der UI angezeigt, mit welchem der Benutzer sich in der zeitlichen Ebene bewegen kann. Die Navigation durch den Slider ersetzt die oberste Ebene in oben beschriebener Version 1.

## Komponente 1: Updater

Der Updater läuft kontinuierlich und ruft in einem gegebenen Interval einen neuen Datensatz für die Visualisierung ab, welcher dann als JSON Datei serialisiert lokal gespeichert wird. Daten werden durch die Twitter REST API abgerufen.

Aufgrund von Beschränkungen in der Twitter API werden hierbei die Trending Topics für den geographischen Raum Deutschland verwendet. Um Daten für die zweite Ebene der Visualisierung zu gewinnen, wird eine Suche nach dem Hashtag durchgeführt und die Häufigkeit anderer Hashtags in den Suchergebnissen wird aufsummiert.

Nach jedem Update wird das Datenmodell der parallel laufenden *main app* aktualisiert, so dass neue Daten hier nach einem *Browser page* reload zur Verfügung stehen.

## Komponente 2: Main App

Die TweetCone App lädt nach Programmstart bestehende Datensätze im JSON Format mithilfe der AESON library und geeigneter Haskell Datentypen ein. Diese werden dann in einen ConeTree entsprecht Abschnitt Beschreibung der Visualisierung übersetzt. Dieser ConeTree wird dem ConeServer übergeben, welcher ihn als ConeCanvas präsentiert. 

## Lizenz

All Rights Reserved (c) 2016 Symbolian GmbH
