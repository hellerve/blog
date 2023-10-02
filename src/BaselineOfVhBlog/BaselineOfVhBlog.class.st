Class {
	#name : #BaselineOfVhBlog,
	#superclass : #BaselineOf,
	#category : #BaselineOfVhBlog
}

{ #category : #accessing }
BaselineOfVhBlog class >> loadLepiter [
	IceRepository registry  
		detect: [ :aRepository |
			aRepository name = 'blog' ]
		ifFound: [ :aRepository | 
			| defaultDatabase currentProperties |
			defaultDatabase := LeDatabasesRegistry defaultLogicalDatabase.
			currentProperties := defaultDatabase properties.
			currentProperties addRegisteredDirectory: aRepository repositoryDirectory / 'lepiter'.
			defaultDatabase reload ]
		ifNone: [
			self inform: 'Repository not found.' ]
]

{ #category : #baseline }
BaselineOfVhBlog >> baseline: spec [
	<baseline>
		^ spec for: #common do: [
			spec package: 'VhBlog'
		]
]
