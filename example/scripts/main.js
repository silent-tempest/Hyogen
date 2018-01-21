;( function () {

'use strict';

var main = function ( code ) {
  try {
    new hyogen.Runtime()[ 'eval' ]( code );
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
