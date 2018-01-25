;( function () {

'use strict';

var output = document.getElementById( 'output' );

hyogen.settings.print = function ( value ) {
  output.value += value + '\n';
};

var main = function ( code ) {
  new hyogen.Runtime()[ 'eval' ]( code );
};

_.fetch( 'code/import.hg' )
  .then( function ( res ) {
    return res.text();
  } )
  .then( main );

} )();
