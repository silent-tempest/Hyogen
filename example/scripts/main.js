;( function () {

'use strict';

var output = document.getElementById( 'output' );

hyogen.settings.print = function ( value ) {
  output.value += value + '\n';
};

var main = function ( code ) {
  try {
    alert( JSON.stringify( new hyogen.Runtime()[ 'eval' ]( code ), null, '\t' ) );
  } catch ( ex ) {
    alert( ex );
  }
};

_.fetch( 'code/import.hg' )
  .then( function ( res ) {
    return res.text();
  } )
  .then( main );

} )();
