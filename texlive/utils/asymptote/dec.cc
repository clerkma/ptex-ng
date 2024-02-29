/*****
 * dec.cc
 * Andy Hammerlindl 2002/8/29
 *
 * Represents the abstract syntax tree for declarations in the language.
 * Also included is an abstract syntax for types as they are most often
 * used with declarations.
 *****/

#include "errormsg.h"
#include "coenv.h"
#include "dec.h"
#include "fundec.h"
#include "newexp.h"
#include "stm.h"
#include "exp.h"
#include "modifier.h"
#include "runtime.h"
#include "locate.h"
#include "parser.h"
// #include "builtin.h"  // for trans::addRecordOps

namespace absyntax {

using namespace trans;
using namespace types;

using mem::list;

trans::tyEntry *ty::transAsTyEntry(coenv &e, record *where)
{
  return new trans::tyEntry(trans(e, false), 0, where, getPos());
}


void nameTy::prettyprint(ostream &out, Int indent)
{
  prettyname(out, "nameTy",indent, getPos());

  id->prettyprint(out, indent+1);
}

types::ty *nameTy::trans(coenv &e, bool tacit)
{
  return id->typeTrans(e, tacit);
}

trans::tyEntry *nameTy::transAsTyEntry(coenv &e, record *)
{
  return id->tyEntryTrans(e);
}

nameTy::operator string() const
{
  return static_cast<string>(id->getName());
}

void dimensions::prettyprint(ostream &out, Int indent)
{
  prettyindent(out, indent);
  out << "dimensions (" << depth << ")\n";
}

types::array *dimensions::truetype(types::ty *base, bool tacit)
{
  if (!tacit && base->kind == ty_void) {
    em.error(getPos());
    em << "cannot declare array of type void";
  }

  assert(depth >= 1);
  size_t d=depth;
  types::array *a=new types::array(base); d--;
  for (; d > 0; d--) {
    a = new types::array(a);
  }
  return a;
}


void arrayTy::prettyprint(ostream &out, Int indent)
{
  prettyname(out, "arrayTy",indent, getPos());

  cell->prettyprint(out, indent+1);
  dims->prettyprint(out, indent+1);
}

// NOTE: Can this be merged with trans somehow?
void arrayTy::addOps(coenv &e, record *r)
{
  types::ty *t=trans(e, true);

  // Only add ops if it is an array (and not, say, an error)
  if (t->kind == types::ty_array) {
    types::array *at=dynamic_cast<types::array *>(t);
    assert(at);
    e.e.addArrayOps(at);
    if (r)
      r->e.addArrayOps(at);
  }
}

types::ty *arrayTy::trans(coenv &e, bool tacit)
{
  types::ty *ct = cell->trans(e, tacit);
  assert(ct);

  // Don't make an array of errors.
  if (ct->kind == types::ty_error)
    return ct;

  types::array *t = dims->truetype(ct,tacit);
  assert(t);

  return t;
}

arrayTy::operator string() const
{
  stringstream ss;
  ss << static_cast<string>(*cell);
  for (size_t i = 0; i < dims->size(); i++)
  {
    ss << "[]";
  }
  return ss.str();
}

tyEntryTy::tyEntryTy(position pos, types::ty *t)
  : ty(pos), ent(new trans::tyEntry(t, 0, 0, position()))
{
}

void tyEntryTy::prettyprint(ostream &out, Int indent)
{
  prettyindent(out,indent);
  out << "tyEntryTy: " << *(ent->t) << "\n";
}

types::ty *tyEntryTy::trans(coenv &, bool) {
  return ent->t;
}

tyEntryTy::operator string() const
{
  return "<unknown-type>";
}

vm::lambda *runnable::transAsCodelet(coenv &e)
{
  coder c=e.c.newCodelet(getPos());
  coenv ce(c, e.e);
  markTrans(ce);
  return c.close();
}


void block::prettystms(ostream &out, Int indent)
{
  for (list<runnable *>::iterator p = stms.begin(); p != stms.end(); ++p)
    (*p)->prettyprint(out, indent);
}

void block::prettyprint(ostream &out, Int indent)
{
  prettyname(out,"block",indent,getPos());
  prettystms(out, indent+1);
}

// Uses RAII to ensure scope is ended when function returns.
class Scope {
  coenv* e;
public:
  Scope(coenv &e, bool scope) : e(scope ? &e : nullptr) {
    if (this->e) this->e->e.beginScope();
  }
  ~Scope() {
    if (this->e) e->e.endScope();
  }
};


void block::trans(coenv &e)
{
  Scope scopeHolder(e, scope);
  for (list<runnable *>::iterator p = stms.begin(); p != stms.end(); ++p) {
    (*p)->markTrans(e);
  }
}

void block::transAsField(coenv &e, record *r)
{
  Scope scopeHolder(e, scope);
  for (list<runnable *>::iterator p = stms.begin(); p != stms.end(); ++p) {
    (*p)->markTransAsField(e, r);
    if (em.errors() && !settings::getSetting<bool>("debug"))
      break;
  }
}

bool block::transAsTemplatedField(
    coenv &e, record *r, mem::vector<absyntax::namedTyEntry*>* args
) {
  Scope scopeHolder(e, scope);
  auto p = stms.begin();
  if (p == stms.end()) {
    return true;  // empty file
  }
  receiveTypedefDec *dec = dynamic_cast<receiveTypedefDec *>(*p);
  if (!dec) {
    em.error(getPos());
    em << "expected 'typedef import(<types>);'";
    em.sync();
    return false;
  }
  if(!dec->transAsParamMatcher(e, r, args))
    return false;

  while (++p != stms.end()) {
    (*p)->markTransAsField(e, r);
    if (em.errors() && !settings::getSetting<bool>("debug")) {
      return false;
    }
  }
  em.sync();
  return true;
}

void block::transAsRecordBody(coenv &e, record *r)
{
  transAsField(e, r);
  e.c.closeRecord();
}

bool block::transAsTemplatedRecordBody(
    coenv &e, record *r, mem::vector<absyntax::namedTyEntry*> *args
) {
  bool succeeded = transAsTemplatedField(e, r, args);
  e.c.closeRecord();
  return succeeded;
}

record *block::transAsFile(genv& ge, symbol id)
{
  // Create the new module.
  record *r = new record(id, new frame(id,0,0));

  // Create coder and environment to translate the module.
  // File-level modules have dynamic fields by default.
  coder c(getPos(), r, 0);
  env e(ge);
  coenv ce(c, e);

  // Translate the abstract syntax.
  if (settings::getSetting<bool>("autoplain")) {
    autoplainRunnable()->transAsField(ce, r);
  }
  transAsRecordBody(ce, r);
  em.sync();
  if (em.errors()) return nullptr;

  return r;
}

record *block::transAsTemplatedFile(
    genv& ge, symbol id, mem::vector<absyntax::namedTyEntry*>* args
) {
  // Create the new module.
  record *r = new record(id, new frame(id,0,0));

  // Create coder and environment to translate the module.
  // File-level modules have dynamic fields by default.
  coder c(getPos(), r, 0);
  env e(ge);
  coenv ce(c, e);

  // Translate the abstract syntax.
  if (settings::getSetting<bool>("autoplain")) {
    autoplainRunnable()->transAsField(ce, r);
  }

  bool succeeded = transAsTemplatedRecordBody(ce, r, args);
  if (!succeeded) {
    return nullptr;
  }

  return r;
}


bool block::returns() {
  // Search for a returning runnable, starting at the end for efficiency.
  for (list<runnable *>::reverse_iterator p=stms.rbegin();
       p != stms.rend();
       ++p)
    if ((*p)->returns())
      return true;
  return false;
}

vardec *block::asVardec()
{
  vardec *var = 0;
  for (list<runnable *>::iterator p=stms.begin();
       p != stms.end();
       ++p)
    {
      vardec *v = dynamic_cast<vardec *>(*p);
      if (v) {
        if (var)
          // Multiple vardecs.
          return 0;
        var = v;
      }
      else if (!dynamic_cast<emptyStm *>(*p))
        // Failure due to another runnable in the block.
        return 0;
    }

  return var;
}

void block::createSymMap(AsymptoteLsp::SymbolContext* symContext) {
#ifdef HAVE_LSP
  for (auto const& p : stms)
  {
    p->createSymMap(symContext);
  }
#endif
}

void dec::prettyprint(ostream &out, Int indent)
{
  prettyname(out, "dec", indent, getPos());
}


void modifierList::prettyprint(ostream &out, Int indent)
{
  prettyindent(out,indent);
  out << "modifierList (";

  for (list<modifier>::iterator p = mods.begin(); p != mods.end(); ++p) {
    if (p != mods.begin())
      out << ", ";
    switch (*p) {
      case EXPLICIT_STATIC:
        out << "static";
        break;
#if 0
      case EXPLICIT_DYNAMIC:
        out << "dynamic";
        break;
#endif
      default:
        out << "invalid code";
    }
  }

  for (list<permission>::iterator p = perms.begin(); p != perms.end(); ++p) {
    if (p != perms.begin() || !mods.empty())
      out << ", ";
    switch (*p) {
      case PUBLIC:
        out << "public";
        break;
      case PRIVATE:
        out << "private";
        break;
      default:
        out << "invalid code";
    }
  }

  out << ")\n";
}

bool modifierList::staticSet()
{
  return !mods.empty();
}

modifier modifierList::getModifier()
{
  if (mods.size() > 1) {
    em.error(getPos());
    em << "too many modifiers";
  }

  assert(staticSet());
  return mods.front();
}

permission modifierList::getPermission()
{
  if (perms.size() > 1) {
    em.error(getPos());
    em << "too many modifiers";
  }

  return perms.empty() ? DEFAULT_PERM : perms.front();
}


void modifiedRunnable::prettyprint(ostream &out, Int indent)
{
  prettyname(out, "modifierRunnable",indent, getPos());

  mods->prettyprint(out, indent+1);
  body->prettyprint(out, indent+1);
}

void modifiedRunnable::transAsField(coenv &e, record *r)
{
  if (mods->staticSet()) {
    if (e.c.isTopLevel()) {
      em.warning(getPos());
      em << "static modifier is meaningless at top level";
    }
    e.c.pushModifier(mods->getModifier());
  }

  permission p = mods->getPermission();
#if 0 // This is innocuous
  if (p != DEFAULT_PERM && (!r || !body->allowPermissions())) {
    em.warning(pos);
    em << "permission modifier is meaningless";
  }
#endif
  e.c.setPermission(p);

  body->transAsField(e,r);

  e.c.clearPermission();
  if (mods->staticSet())
    e.c.popModifier();
}


void decidstart::prettyprint(ostream &out, Int indent)
{
  prettyindent(out, indent);
  out << "decidstart '" << id << "'\n";

  if (dims)
    dims->prettyprint(out, indent+1);
}

types::ty *decidstart::getType(types::ty *base, coenv &, bool)
{
  return dims ? dims->truetype(base) : base;
}

trans::tyEntry *decidstart::getTyEntry(trans::tyEntry *base, coenv &e,
                                       record *where)
{
  return dims ? new trans::tyEntry(getType(base->t,e,false), 0,
                                   where, getPos()) :
    base;
}

void decidstart::addOps(types::ty *base, coenv &e, record *r)
{
  if (dims) {
    array *a=dims->truetype(base);
    e.e.addArrayOps(a);
    if (r)
      r->e.addArrayOps(a);
  }
}

void decidstart::createSymMap(AsymptoteLsp::SymbolContext* symContext)
{
#ifdef HAVE_LSP
  std::string name(static_cast<std::string>(getName()));
  AsymptoteLsp::posInFile pos(getPos().LineColumn());
  if (auto decCtx=dynamic_cast<AsymptoteLsp::AddDeclContexts*>(symContext))
  {
    decCtx->additionalDecs.emplace(std::piecewise_construct,
            std::forward_as_tuple(name), std::forward_as_tuple(name, pos));
  }
  else
  {
    symContext->symMap.varDec[name] = AsymptoteLsp::SymbolInfo(name, pos);
  }
#endif
}

void decidstart::createSymMapWType(
    AsymptoteLsp::SymbolContext* symContext, absyntax::ty* base
) {
#ifdef HAVE_LSP
  std::string name(static_cast<std::string>(getName()));
  AsymptoteLsp::posInFile pos(getPos().LineColumn());
  if (auto decCtx=dynamic_cast<AsymptoteLsp::AddDeclContexts*>(symContext))
  {
    if (base == nullptr)
    {
      decCtx->additionalDecs.emplace(std::piecewise_construct,
                                     std::forward_as_tuple(name),
                                     std::forward_as_tuple(name, pos));
    }
    else
    {
      decCtx->additionalDecs.emplace(
          std::piecewise_construct,
          std::forward_as_tuple(name),
          std::forward_as_tuple(name, static_cast<std::string>(*base), pos)
      );
    }
  }
  else
  {
    symContext->symMap.varDec[name] = base == nullptr ?
            AsymptoteLsp::SymbolInfo(name, pos) :
            AsymptoteLsp::SymbolInfo(name,
                                     static_cast<std::string>(*base),
                                     pos);
  }
#endif
}


  void fundecidstart::prettyprint(ostream &out, Int indent)
{
  prettyindent(out, indent);
  out << "fundecidstart '" << id << "'\n";

  if (dims)
    dims->prettyprint(out, indent+1);
  if (params)
    params->prettyprint(out, indent+1);
}

types::ty *fundecidstart::getType(types::ty *base, coenv &e, bool tacit)
{
  types::ty *result = decidstart::getType(base, e, tacit);

  if (params) {
    return params->getType(result, e, true, tacit);
  }
  else {
    types::ty *t = new function(base);
    return t;
  }
}

trans::tyEntry *fundecidstart::getTyEntry(trans::tyEntry *base, coenv &e,
                                          record *where)
{
  return new trans::tyEntry(getType(base->t,e,false), 0, where, getPos());
}

void fundecidstart::addOps(types::ty *base, coenv &e, record *r)
{
  decidstart::addOps(base, e, r);

  params->addOps(e, r);

  types::function *ft=dynamic_cast<types::function *>(getType(base, e, true));
  assert(ft);

  e.e.addFunctionOps(ft);
  if (r)
    r->e.addFunctionOps(ft);
}


void decid::prettyprint(ostream &out, Int indent)
{
  prettyname(out, "decid",indent, getPos());

  start->prettyprint(out, indent+1);
  if (init)
    init->prettyprint(out, indent+1);
}


varEntry *makeVarEntryWhere(coenv &e, record *r, types::ty *t,
                            record *where, position pos)
{
  access *a = r ? r->allocField(e.c.isStatic()) :
    e.c.allocLocal();

  return r ? new varEntry(t, a, e.c.getPermission(), r, where, pos) :
    new varEntry(t, a, where, pos);
}

varEntry *makeVarEntry(position pos, coenv &e, record *r, types::ty *t) {
  return makeVarEntryWhere(e, r, t, r, pos);
}


// Defined in constructor.cc.
bool definesImplicitConstructor(coenv &e, record *r, varEntry *v, symbol id);
void addConstructorFromInitializer(position pos, coenv &e, record *r,
                                   varEntry *init);

void addVar(coenv &e, record *r, varEntry *v, symbol id)
{
  // Test for 'operator init' definitions that implicitly define constructors:
  if (definesImplicitConstructor(e, r, v, id))
    addConstructorFromInitializer(position(), e, r, v);

  // Add to the record so it can be accessed when qualified; add to the
  // environment so it can be accessed unqualified in the scope of the
  // record definition.
  if (r)
    r->e.addVar(id, v);
  e.e.addVar(id, v);
}

void initializeVar(position pos, coenv &e, varEntry *v, varinit *init)
{
  types::ty *t=v->getType();

  if (init)
    init->transToType(e, t);
  else {
    definit d(pos);
    d.transToType(e, t);
  }

  v->getLocation()->encode(WRITE, pos, e.c);
  e.c.encodePop();
}

types::ty *inferType(position pos, coenv &e, varinit *init)
{
  if (!init) {
    em.error(pos);
    em << "inferred variable declaration without initializer";
    return primError();
  }

  exp *base = dynamic_cast<exp *>(init);
  bool Void=false;

  if (base) {
    types::ty *t = base->cgetType(e);
    Void=t->kind == ty_void;
    if (t->kind != ty_overloaded && !Void)
      return t;
  }

  em.error(pos);
  em << (Void ? "cannot infer from void" :
         "could not infer type of initializer");

  return primError();
}

void createVar(position pos, coenv &e, record *r,
               symbol id, types::ty *t, varinit *init)
{
  // I'm not sure how to handle inferred types in these cases.
  assert(t->kind != types::ty_inferred);

  varEntry *v=makeVarEntry(pos, e, r, t);
  addVar(e, r, v, id);
  initializeVar(pos, e, v, init);
}

void createVarOutOfOrder(position pos, coenv &e, record *r,
                         symbol id, types::ty *t, varinit *init)
{
  /* For declarations such as "var x = 5;", infer the type from the
   * initializer.
   */
  if (t->kind == types::ty_inferred)
    t = inferType(pos, e, init);

  varEntry *v=makeVarEntry(pos, e, r, t);
  initializeVar(pos, e, v, init);
  addVar(e, r, v, id);
}

void addTypeWithPermission(coenv &e, record *r, tyEntry *base, symbol id)
{
  // Only bother encoding permissions for private types.
  tyEntry *ent = (r && e.c.getPermission()==PRIVATE) ?
    new trans::tyEntry(base, PRIVATE, r) :
    base;

  if (r)
    r->e.addType(id, ent);
  e.e.addType(id, ent);
}


void decid::transAsField(coenv &e, record *r, types::ty *base)
{
  types::ty *t = start->getType(base, e);
  assert(t);
  if (t->kind == ty_void) {
    em.error(getPos());
    em << "cannot declare variable of type void";
  }

  start->addOps(base, e, r);

  createVarOutOfOrder(getPos(), e, r, start->getName(), t, init);
}

void decid::transAsTypedefField(coenv &e, trans::tyEntry *base, record *r)
{
  trans::tyEntry *ent = start->getTyEntry(base, e, r);
  assert(ent && ent->t);

  if (init) {
    em.error(getPos());
    em << "type definition cannot have initializer";
  }

  start->addOps(base->t, e, r);

  addTypeWithPermission(e, r, ent, start->getName());
}

void decid::createSymMap(AsymptoteLsp::SymbolContext* symContext)
{
#ifdef HAVE_LSP
  start->createSymMap(symContext);
  if (init)
  {
    init->createSymMap(symContext);
  }
#endif
}

void decid::createSymMapWType(
    AsymptoteLsp::SymbolContext* symContext, absyntax::ty* base
) {
#ifdef HAVE_LSP
  start->createSymMapWType(symContext, base);
  if (init)
  {
    init->createSymMap(symContext);
  }
#endif
}


void decidlist::prettyprint(ostream &out, Int indent)
{
  prettyname(out, "decidlist",indent, getPos());

  for (list<decid *>::iterator p = decs.begin(); p != decs.end(); ++p)
    (*p)->prettyprint(out, indent+1);
}

void decidlist::transAsField(coenv &e, record *r, types::ty *base)
{
  for (list<decid *>::iterator p = decs.begin(); p != decs.end(); ++p)
    (*p)->transAsField(e, r, base);
}

void decidlist::transAsTypedefField(coenv &e, trans::tyEntry *base, record *r)
{
  for (list<decid *>::iterator p = decs.begin(); p != decs.end(); ++p)
    (*p)->transAsTypedefField(e, base, r);
}

void decidlist::createSymMap(AsymptoteLsp::SymbolContext* symContext) {
#ifdef HAVE_LSP
  for (auto const& p : decs)
  {
    p->createSymMap(symContext);
  }
#endif
}

void decidlist::createSymMapWType(
    AsymptoteLsp::SymbolContext* symContext, absyntax::ty* base
) {
#ifdef HAVE_LSP
  for (auto const& p : decs)
  {
    p->createSymMapWType(symContext, base);
  }
#endif
}


void vardec::prettyprint(ostream &out, Int indent)
{
  prettyname(out, "vardec",indent, getPos());

  base->prettyprint(out, indent+1);
  decs->prettyprint(out, indent+1);
}

void vardec::transAsTypedefField(coenv &e, record *r)
{
  base->addOps(e, r);
  decs->transAsTypedefField(e, base->transAsTyEntry(e, r), r);
}

symbol vardec::singleName()
{
  decid *did = decs->singleEntry();
  if (!did)
    return symbol::nullsym;
  return did->getStart()->getName();
}

types::ty *vardec::singleGetType(coenv &e)
{
  decid *did = decs->singleEntry();
  if (!did)
    return 0;
  return did->getStart()->getType(base->trans(e), e);
}

void vardec::createSymMap(AsymptoteLsp::SymbolContext* symContext)
{
#ifdef HAVE_LSP
  decs->createSymMapWType(symContext, base);
#endif
}


// Helper class for imports.  This essentially evaluates to the run::loadModule
// function.  However, that function returns different types of records
// depending on the filename given to it, so we cannot add it to the
// environment.  Instead, we explicitly tell it what types::record it is
// returning for each use.
class loadModuleExp : public exp {
  function *ft;

public:
  loadModuleExp(position pos, record *imp)
    : exp(pos) {ft=new function(imp,primString(),primString());}

  void prettyprint(ostream &out, Int indent) {
    prettyname(out, "loadModuleExp", indent, getPos());
  }

  types::ty *trans(coenv &) {
    em.compiler(getPos());
    em << "trans called for loadModuleExp";
    return primError();
  }

  void transCall(coenv &e, types::ty *t) {
    assert(equivalent(t, ft));
    e.c.encode(inst::builtin, run::loadModule);
  }

  types::ty *getType(coenv &) {
    return ft;
  }

  exp *evaluate(coenv &, types::ty *) {
    // Don't alias.
    return this;
  }
};

// Creates a local variable to hold the import and translate the accessing of
// the import, but doesn't add the import to the environment.
varEntry *accessModule(position pos, coenv &e, record *r, symbol id)
{
  record *imp=e.e.getModule(id, (string)id);
  if (!imp) {
    em.error(pos);
    em << "could not load module '" << id << "'";
    em.sync();
    return 0;
  }
  else {
    // Create a varinit that evaluates to the module.
    // This is effectively the expression 'loadModule(filename,"")'.
    callExp init(pos, new loadModuleExp(pos, imp),
                 new stringExp(pos, (string)id), new stringExp(pos, ""));

    // The varEntry should have whereDefined()==0 as it is not defined inside
    // the record r.
    varEntry *v=makeVarEntryWhere(e, r, imp, 0, pos);
    initializeVar(pos, e, v, &init);
    return v;
  }
}

// Creates a local variable to hold the import and translate the accessing of
// the import, but doesn't add the import to the environment.
varEntry *accessTemplatedModule(position pos, coenv &e, record *r, symbol id,
                                formals *args)
{
  stringstream s;
  s << args->getSignature(e)->handle();
  string sigHandle=s.str();

  auto *computedArgs = new mem::vector<namedTyEntry*>();
  mem::vector<tySymbolPair> *fields = args->getFields();
  for (auto p = fields->begin(); p != fields->end(); ++p) {
    ty* theType = p->ty;
    symbol theName = p->sym;
    if (theName == symbol::nullsym) {
      em.error(theType->getPos());
      em << "expected typename=";
      em.sync();
      return nullptr;
    }
    computedArgs->push_back(new namedTyEntry(
        theType->getPos(), theName, theType->transAsTyEntry(e, r)
    ));
  }

  record *imp=e.e.getTemplatedModule(id, (string) id, sigHandle, computedArgs);
  if (!imp) {
    em.error(pos);
    em << "could not load module '" << id << "'";
    em.sync();
    return nullptr;
  }
  else {
    // Create a varinit that evaluates to the module.
    // This is effectively the expression 'loadModule(filename,index)'.
    callExp init(pos, new loadModuleExp(pos, imp),
                 new stringExp(pos, (string) id), new stringExp(pos, sigHandle));

    // The varEntry should have whereDefined()==0 as it is not defined inside
    // the record r.
    varEntry *v=makeVarEntryWhere(e, r, imp, 0, pos);
    initializeVar(pos, e, v, &init);
    return v;
  }
}


void idpair::prettyprint(ostream &out, Int indent)
{
  prettyindent(out, indent);
  out << "idpair (" << "'" << src << "' as " << dest << ")\n";
}

void idpair::transAsAccess(coenv &e, record *r)
{
  checkValidity();

  varEntry *v=accessModule(getPos(), e, r, src);
  if (v)
    addVar(e, r, v, dest);
}

void idpair::transAsUnravel(coenv &e, record *r,
                            protoenv &source, varEntry *qualifier)
{
  checkValidity();

  if (r)
    r->e.add(src, dest, source, qualifier, e.c);
  if (!e.e.add(src, dest, source, qualifier, e.c)) {
    em.error(getPos());
    em << "no matching types or fields of name '" << src << "'";
  }
}

void idpair::createSymMap(AsymptoteLsp::SymbolContext* symContext)
{
#ifdef HAVE_LSP
  if (valid)
  {
    string fullSrc(settings::locateFile(src, true));
    if (not AsymptoteLsp::isVirtualFile((std::string)(fullSrc.c_str())))
    {
      if (not fullSrc.empty())
      {
        symContext->addEmptyExtRef((std::string)(fullSrc.c_str()));
      }

      // add (dest, source) to reference map.
      auto s = symContext->extRefs.fileIdPair.emplace(
          dest, (std::string) fullSrc.c_str()
      );
      auto it=std::get<0>(s);
      auto success=std::get<1>(s);
      if (not success)
      {
        it->second = (std::string)(fullSrc.c_str());
      }

      symContext->extRefs.addAccessVal(static_cast<std::string>(dest));
    }
  }
#endif
}


void idpairlist::prettyprint(ostream &out, Int indent)
{
  for (list<idpair *>::iterator p=base.begin();
       p != base.end();
       ++p)
    (*p)->prettyprint(out, indent);
}

void idpairlist::transAsAccess(coenv &e, record *r)
{
  for (list<idpair *>::iterator p=base.begin();
       p != base.end();
       ++p)
    (*p)->transAsAccess(e,r);
}

void idpairlist::transAsUnravel(coenv &e, record *r,
                                protoenv &source, varEntry *qualifier)
{
  for (list<idpair *>::iterator p=base.begin();
       p != base.end();
       ++p)
    (*p)->transAsUnravel(e,r,source,qualifier);
}

void idpairlist::createSymMap(AsymptoteLsp::SymbolContext* symContext)
{
#ifdef HAVE_LSP
  for (auto& idp : base)
  {
    idp->createSymMap(symContext);
  }
#endif
}

  idpairlist * const WILDCARD = 0;

void accessdec::prettyprint(ostream &out, Int indent)
{
  prettyname(out, "accessdec", indent, getPos());
  base->prettyprint(out, indent+1);
}

void accessdec::createSymMap(AsymptoteLsp::SymbolContext* symContext)
{
#ifdef HAVE_LSP
  base->createSymMap(symContext);
#endif
}

void templateAccessDec::transAsField(coenv& e, record* r) {
  if (!this->checkValidity()) return;

  args->addOps(e, r);

  varEntry *v=accessTemplatedModule(getPos(), e, r, this->src, args);
  if (v)
    addVar(e, r, v, dest);
}

void typeParam::prettyprint(ostream &out, Int indent) {
  prettyindent(out, indent);
  out << "typeParam (" << paramSym <<  ")\n";
}

bool typeParam::transAsParamMatcher(coenv &e, namedTyEntry* arg) {
  if (arg->dest != paramSym) {
    em.error(arg->pos);
    em << "template argument name does not match module: passed "
       << arg->dest
       << ", expected "
       << paramSym;
    return false;
  }
  e.e.addType(paramSym, arg->ent);
  // The code below would add e.g. operator== to the context, but potentially
  // ignore overrides of operator==:
  //
  // types::ty *t = arg.ent->t;
  // if (t->kind == types::ty_record) {
  //   record *r = dynamic_cast<record *>(t);
  //   if (r) {
  //     trans::addRecordOps(e.e.ve, r);
  //   }
  // } else if (t->kind == types::ty_array) {
  //   array *a = dynamic_cast<array *>(t);
  //   if (a) {
  //     trans::addArrayOps(e.e.ve, a);
  //   }
  // } else if (t->kind == types::ty_function) {
  //   function *f = dynamic_cast<function *>(t);
  //   if (f) {
  //     trans::addFunctionOps(e.e.ve, f);
  //   }
  // }
  return true;
}

void typeParamList::prettyprint(ostream &out, Int indent) {
  for (auto p = params.begin(); p != params.end(); ++p) {
    (*p)->prettyprint(out, indent);
  }
}

void typeParamList::add(typeParam *tp) {
  params.push_back(tp);
}

bool typeParamList::transAsParamMatcher(
    coenv &e, mem::vector<namedTyEntry*> *args
) {
  if (args->size() != params.size()) {
    position pos = getPos();
    if (args->size() >= 1) {
      pos = (*args)[0]->pos;
    }
    em.error(pos);
    if (args->size() > params.size()) {
      em << "too many types passed: got " << args->size() << ", expected "
         << params.size();
    } else {
      em << "too few types passed: got " << args->size() << ", expected "
         << params.size();
    }
    return false;
  }
  for (size_t i = 0; i < params.size(); ++i) {
    bool succeeded = params[i]->transAsParamMatcher(e, (*args)[i]);
    if (!succeeded) return false;
  }
  return true;
}

symbol intSymbol() {
  const static symbol* intSymbol = new symbol(symbol::literalTrans("int"));
  return *intSymbol;
}

symbol templatedSymbol() {
  const static symbol* templatedSymbol =
      new symbol(symbol::literalTrans("/templated"));
  return *templatedSymbol;
}

bool receiveTypedefDec::transAsParamMatcher(
    coenv& e, record *r, mem::vector<namedTyEntry*> *args
) {
  bool succeeded = params->transAsParamMatcher(e, args);

  types::ty *intTy = e.e.lookupType(intSymbol());
  assert(intTy);
  e.e.addVar(templatedSymbol(),
             makeVarEntryWhere(e, nullptr, intTy, r, getPos())
            );

  return succeeded;
}

void receiveTypedefDec::transAsField(coenv& e, record *r) {
  em.error(getPos());
  types::ty *intTy = e.e.lookupType(intSymbol());
  assert(intTy);
  if (e.e.lookupVarByType(templatedSymbol(), intTy)) {
    em << "'typedef import(<types>)' must precede any other code";
  } else {
    em << "templated module access requires template parameters";
  }
  em.sync();
}


void fromdec::prettyprint(ostream &out, Int indent)
{
  prettyname(out, "fromdec", indent, getPos());
  fields->prettyprint(out, indent+1);
}

void fromdec::transAsField(coenv &e, record *r)
{
  qualifier q=getQualifier(e,r);
  if (q.t) {
    if (fields==WILDCARD) {
      if (r)
        r->e.add(q.t->e, q.v, e.c);
      e.e.add(q.t->e, q.v, e.c);
    }
    else
      fields->transAsUnravel(e, r, q.t->e, q.v);
  }
}


void unraveldec::prettyprint(ostream &out, Int indent)
{
  prettyname(out, "unraveldec", indent, getPos());
  id->prettyprint(out, indent+1);
  idpairlist *f=this->fields;
  if(f) f->prettyprint(out, indent+1);
}

fromdec::qualifier unraveldec::getQualifier(coenv &e, record *)
{
  // getType is where errors in the qualifier are reported.
  record *qt=dynamic_cast<record *>(id->getType(e, false));
  if (!qt) {
    em.error(getPos());
    em << "qualifier is not a record";
  }

  return qualifier(qt,id->getVarEntry(e));
}

void unraveldec::createSymMap(AsymptoteLsp::SymbolContext* symContext)
{
#ifdef HAVE_LSP
  std::string fileName = static_cast<std::string>(id->getName());
  if (not AsymptoteLsp::isVirtualFile(fileName))
  {
    symContext->extRefs.addUnravelVal(fileName);
  }
#endif
}

void fromaccessdec::prettyprint(ostream &out, Int indent)
{
  prettyindent(out, indent);
  out << "fromaccessdec '" << id << "'\n";
  idpairlist *f=this->fields;
  if(f) f->prettyprint(out, indent+1);
}

fromdec::qualifier fromaccessdec::getQualifier(coenv &e, record *r)
{
  varEntry *v = 0;
  if (templateArgs) {
    v = accessTemplatedModule(getPos(), e, r, id, templateArgs);
  } else {
    v=accessModule(getPos(), e, r, id);
  }
  if (v) {
    record *qt=dynamic_cast<record *>(v->getType());
    if (!qt) {
      em.compiler(getPos());
      em << "qualifier is not a record";
    }
    return qualifier(qt, v);
  }
  else
    return qualifier(0,0);
}

void fromaccessdec::createSymMap(AsymptoteLsp::SymbolContext* symContext)
{
#ifdef HAVE_LSP
  // filename is id;
  std::string idStr(id);
  symContext->extRefs.addEmptyExtRef(idStr);

  auto* f=this->fields;
  if (f)
  {
    // add [dest] -> [src, filename] to fromAccessDecls;
    f->processListFn(
            [&symContext, &idStr](symbol const& src, symbol const& dest)
            {
              std::string srcId(src);
              std::string destId(dest);
              symContext->extRefs.addFromAccessVal(idStr, srcId, destId);
            });
  }
#endif
}

void importdec::prettyprint(ostream &out, Int indent)
{
  prettyname(out, "importdec", indent, getPos());
  base.prettyprint(out, indent+1);
}

void importdec::createSymMap(AsymptoteLsp::SymbolContext* symContext)
{
#ifdef HAVE_LSP
  base.createSymMap(symContext);
#endif
}

void includedec::prettyprint(ostream &out, Int indent)
{
  prettyindent(out, indent);
  out << "includedec ('" << filename << "')\n";
}

void includedec::loadFailed(coenv &)
{
  em.warning(getPos());
  em << "could not parse file of name '" << filename << "'";
  em.sync();
}

void includedec::transAsField(coenv &e, record *r)
{
  file *ast = parser::parseFile(filename,"Including");
  em.sync();

  // The runnables will be translated, one at a time, without any additional
  // scoping.
  ast->transAsField(e, r);
}

void includedec::createSymMap(AsymptoteLsp::SymbolContext* symContext)
{
#ifdef HAVE_LSP
  std::string fullname(
      (std::string) settings::locateFile(filename, true).c_str()
  );
  if (not AsymptoteLsp::isVirtualFile(fullname))
  {
    symContext->addEmptyExtRef(fullname);
    symContext->extRefs.includeVals.emplace(fullname);
  }
#endif
}


void typedec::prettyprint(ostream &out, Int indent)
{
  prettyname(out, "typedec",indent, getPos());

  body->prettyprint(out, indent+1);
}


void recorddec::prettyprint(ostream &out, Int indent)
{
  prettyindent(out, indent);
  out << "structdec '" << id << "'\n";

  body->prettyprint(out, indent+1);
}

void recorddec::transRecordInitializer(coenv &e, record *parent)
{
  position here=getPos();

  // This is equivalent to the code
  //   A operator init() { return new A; }
  // where A is the name of the record.
  formals formals(here);
  simpleName recordName(here, id);
  nameTy result(here, &recordName);
  newRecordExp exp(here, &result);
  returnStm stm(here, &exp);
  fundec init(here, &result, symbol::opTrans("init"), &formals, &stm);

  init.transAsField(e, parent);
}

void recorddec::addPostRecordEnvironment(coenv &e, record *r, record *parent) {
  if (parent)
    parent->e.add(r->postdefenv, 0, e.c);
  e.e.add(r->postdefenv, 0, e.c);
}

void recorddec::transAsField(coenv &e, record *parent)
{
  record *r = parent ? parent->newRecord(id, e.c.isStatic()) :
    e.c.newRecord(id);

  addTypeWithPermission(e, parent, new trans::tyEntry(r,0,parent,getPos()),
                        id);
  e.e.addRecordOps(r);
  if (parent)
    parent->e.addRecordOps(r);

  // Start translating the initializer.
  coder c=e.c.newRecordInit(getPos(), r);
  coenv re(c,e.e);

  body->transAsRecordBody(re, r);

  // After the record is translated, add a default initializer so that a
  // variable of the type of the record is initialized to a new instance by
  // default.
  transRecordInitializer(e, parent);

  // Add types and variables defined during the record that should be added to
  // the enclosing environment.  These are the implicit constructors defined by
  // "operator init".
  addPostRecordEnvironment(e, r, parent);
}

void recorddec::createSymMap(AsymptoteLsp::SymbolContext* symContext)
{
#ifdef HAVE_LSP
  auto* newCtx = symContext->newContext(getPos().LineColumn());
  auto* structTyInfo = symContext->newTypeDec<AsymptoteLsp::StructDecs>(
          static_cast<std::string>(id), getPos().LineColumn());
  if (structTyInfo != nullptr)
  {
    structTyInfo->ctx = newCtx;
    body->createSymMap(newCtx);
  }
  else
  {
    cerr << "Cannot create new struct context" << endl;
  }
#endif
}

runnable *autoplainRunnable() {
  // Abstract syntax for the code:
  //   private import plain;
  position pos=position();
  static importdec ap(pos, new idpair(pos, symbol::trans("plain")));
  static modifiedRunnable mr(pos, trans::PRIVATE, &ap);

  return &mr;
}

} // namespace absyntax
