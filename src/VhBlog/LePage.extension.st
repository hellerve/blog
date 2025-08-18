Extension { #name : #LePage }

{ #category : #'*VhBlog' }
LePage >> gtMarkdownActionFor: anAction [
	<lePageAction>
	^ anAction button
		tooltip: 'Blog Post as Markdown';
		priority: 1;
		label: 'Markdown';
		action: [ :aButton | 
			aButton phlow
				spawnObject: (GtLlmLepiterContentExporter new
						page: self;
						export) ]
]

{ #category : #'*VhBlog' }
LePage >> gtPublishBlogActionFor: anAction [
	<lePageAction>
	^ anAction button
		tooltip: 'Publish Blog Post';
		priority: 1;
		icon: BrGlamorousVectorIcons link;
		action: [ :aButton | aButton phlow spawnObject: (VhBlogExporter new export: self) ]
]
