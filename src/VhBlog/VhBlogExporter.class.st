Class {
	#name : #VhBlogExporter,
	#superclass : #Object,
	#instVars : [
		'sequencer',
		'linksBuilder',
		'layoutFile',
		'blogDirectory'
	],
	#category : #'VhBlog-Exporter'
}

{ #category : #exporting }
VhBlogExporter >> blogDirectory [
	^ blogDirectory ifNil: [ blogDirectory := self defaultBlogDirectory ]
]

{ #category : #exporting }
VhBlogExporter >> defaultBlogDirectory [
	^ FileLocator imageDirectory / 'pharo-local' / 'iceberg' / 'hellerve' / 'blog'
]

{ #category : #exporting }
VhBlogExporter >> defaultLayoutFile [
	^ FileLocator imageDirectory / 'pharo-local' / 'iceberg' / 'hellerve' / 'blog'
		/ 'le_layout.html'
]

{ #category : #exporting }
VhBlogExporter >> export: aPage [
	| pageLinks ourPageLink aContext anExporter htmlString |
	pageLinks := linksBuilder
			database: aPage database;
			build;
			links.
	ourPageLink := pageLinks
			linkForPage: aPage
			ifFound: #yourself
			ifNone: [ self error: 'This should not happen' ].

	aContext := LeHtmlContext new
			page: aPage;
			pageLinks: pageLinks;
			date: Date today;
			sequencer: sequencer.
	anExporter := LeHtmlPageExporter new
			context: aContext;
			piece: (LeHtmlGtBookPiece fromFile: self layoutFile).

	htmlString := anExporter contents.

	aContext assembler assemble.

	LeExportResourcesDirectorySaver new
		resources: aContext resources;
		rootDirectory: self blogDirectory;
		save.

	self blogDirectory
		/ ((aPage title asString copyReplaceAll: ' ' with: '_') , '.md')
		writeStreamDo: [ :aStream | 
			aStream
				nextPutAll: (GtLlmLepiterContentExporter new
						page: aPage;
						export) ].

	^ anExporter
]

{ #category : #initialization }
VhBlogExporter >> initialize [
	sequencer := LeExportUUIDSequencer new.
	linksBuilder := LeExportPageLinksBuilder new
		html;
		sequencer: sequencer
]

{ #category : #exporting }
VhBlogExporter >> layoutFile [
	^ layoutFile ifNil: [ layoutFile := self defaultLayoutFile ]
]
