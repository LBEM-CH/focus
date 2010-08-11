#ifndef _MYVECTOR_
#define _MYVECTOR_ 1
#include <math.h>
#include <iostream>
#include <fstream>
#include <iomanip>
using namespace std;

template <class a,class T>
class CvectorExp;
template <class T>
class CvectorIter;

template <class A,class B,class R>
class OpCross {

public:
    OpCross() { }

    static inline R apply(A a, B b)
    {
		a++;
		b++;
		R tmpa,tmpb;
		tmpa= (R) *a;
		tmpb= (R) *b;
		a++;
		b++;
		return (R)(tmpa*(*b)-tmpb*(*a));
	}
};

template <class A,class B,class R>
class OpAdd {

public:
    OpAdd() { }

    static inline R apply(A a, B b)
    { 
		return (R)(*a+*b); 
	}
};

template <class A,class B,class R>
class OpSub {

public:
    OpSub() { }

    static inline R apply(A a, B b)
    { return (R)(*a-*b); }
};

template <class A,class B,class R>
class OpMul {

public:
    OpMul() { }

    static inline R apply(A a, B b)
    {
		return (R)((*a)*(*b));
	}
};
template <class A,class B,class R>
class OpDiv {

public:
    OpDiv() { }

    static inline R apply(A a, B b)
    { return (R)((*a)/(*b)); }
};


template <class T>
class Cvector
{
private:
	T* data;
	int vardim;
public:
		Cvector():data(NULL),vardim(0){}
		Cvector(int n)
		{
		data=new T[n];
		vardim=n;
		}
		Cvector(Cvector<T> &v)
		{
		vardim=v.dim();
		data=new T[vardim];
		for(int i=0;i<vardim;i++)
				data[i]=v.get(i);
		}
		~Cvector()
		{
			delete []data;
		}
		T& operator[](int n)
		{
			if(n>=vardim)
			{
					cerr << "out of bounds"<<endl;
					return data[0];
			}
			else
				return data[n];
		}
		T get(int n) const
		{
			if(n>=vardim)
			{
					cerr << "out of bounds"<<endl;
					return data[0];
			}
			else
			{
				return data[n];
			}
		}
		template <class A>
		Cvector& operator=(CvectorExp<A,T> result)
		{
			if(result.dim()!=vardim)
			{
				delete []data;
				vardim=result.dim();
				data= new T[vardim];
			}
			CvectorIter<T> iter(*this);
			CvectorIter<T> enditer(*this);
			enditer.end();
            do{
				*iter = *result;   
				result++;
				iter++;
            }while (iter != enditer);
			*iter = *result;   
 			
			return *this;
		}
		Cvector& operator=(const Cvector<T> &v)
		{
			if(v.dim()!=vardim)
			{
				delete []data;
				vardim=v.dim();
				data= new T[vardim];
			}
			CvectorIter<T> aiter(*this),biter(v);
			CvectorIter<T> enditer(*this);
			enditer.end();
            do{
				*aiter = *biter;   
				biter++;
				aiter++;
            }while (aiter != enditer);
			*aiter = *biter;   
 			
			return *this;
		}
		T sum(void) const
		{
			T s;
			s=data[0];
			for(int i=1;i<vardim;i++)
			{
				s=s+data[i];
			}
			return s;
		}
		double length(void) const
		{
			double l=0;
			for(int i=0;i<vardim;i++)
				l+=data[i]*data[i];
			return sqrt(l);
		}
		int dim(void) const
		{
			return vardim;
		}
	
		friend class CvectorIter<T>;
};

template <class T>
class CvectorIter
{
private:
	const Cvector<T>& v;
	int index;
public:
	CvectorIter(const Cvector<T>& vec):v(vec),index(0)
	{
	}
	operator int() const
	{
		return index;
	}
	void operator++()
	{
		if(index<v.vardim-1)
			++index;
		else
			index=0;
	}
		
	void operator++(int n)
	{
		if(index<v.vardim-1)
			index++;
		else
			index=0;
	}
		
	void operator--()
	{
		if(index>0)
			index--;
		else
			end();
	}
		
	void operator--(int n)
	{
		if(index>0)
			index--;
		else
			end();
	}
	T& operator*()
	{
		return v.data[index];
	}
	void begin(void)
	{
		index=0;
	}
	void end(void)
	{
		index=v.vardim-1;
	}
	int dim(void) const
	{
		return v.dim();
	}
};

template <class T>
class CvectorloopIter:public CvectorIter<T>
{
  private:
    const Cvector<T>& v;
    int index;
  public:
	void operator++()
	{
		if(index<v.vardim-1)
			++index;
		else
			index=0;
	}
		
	void operator++(int n)
	{
		if(index<v.vardim-1)
			index++;
		index=0;
	}
		
	void operator--()
	{
		if(index>0)
			index--;
		else
			end();
	}
		
	void operator--(int n)
	{
		if(index>0)
			index--;
		else
			end();
	}

	void end(void)
	{
		index=v.vardim-1;
	}
};
template <class A,class T>
class CvectorExp
{
private:
	A iter;
public:
	CvectorExp(const A& a):iter(a){}
	T operator*() 
	{
		return *iter;
	}
	void operator++()
	{
		iter++;
	}
	void operator++(int n)
	{
		++iter;
	}
	int dim(void) const
	{
		return iter.dim();
	}
	T sum(void)
	{
		Cvector<T> tmp=this;
		return tmp.sum();
	}
	double length(void) const
	{
		Cvector<T> tmp;
		tmp=*this;
		return tmp.length();
	}
};
template<class A, class B, class Op,class T>
class CvectorBinExp 
{

private:
    A iter1;
    B iter2;

public:
    CvectorBinExp(const  A& a,const B& b)
        : iter1(a), iter2(b){ }

    void operator++()
    {
			++iter1;
			++iter2;
	}
   void operator++(int n)
    {
			iter1++;
			iter2++;
	}

    T operator*() 
    { 
		return (T) Op::apply(iter1,iter2);
	}
	int dim(void) const
	{
		int adim=iter1.dim();
		int bdim=iter2.dim();
		return adim>bdim?adim:bdim;
	}
};



template <class T>
class CbuiltinIter
{
private:
	const T t;
public:
	CbuiltinIter(const T bi):t(bi){}
	void operator++(){}
	void operator++(int){}
	void operator--(){}
	void operator--(int){}
	T operator*(){return t;}
	int dim(void)const {return 1;}
};


// Addition
template <class T>
CvectorExp<CvectorBinExp<CvectorIter<T>,CvectorIter<T>,OpAdd<CvectorIter<T>,CvectorIter<T>,T>,T>,T> operator+(const Cvector<T>& a,const Cvector<T>& b)
{
    typedef CvectorBinExp<CvectorIter<T>,CvectorIter<T>,OpAdd<CvectorIter<T>,CvectorIter<T>,T>,T> ExprT;
	CvectorIter<T> aIter(a),bIter(b);
    return CvectorExp<ExprT,T>(ExprT(aIter,bIter));
};


template <class A,class T>
CvectorExp<CvectorBinExp<CvectorExp<A,T>,CvectorIter<T>,OpAdd<CvectorExp<A,T>,CvectorIter<T>,T>,T>,T> operator+(const CvectorExp<A,T>& a,const Cvector<T>& b)
{
		typedef CvectorBinExp<CvectorExp<A,T>,CvectorIter<T>,OpAdd<CvectorExp<A,T>,CvectorIter<T>,T>,T> ExprT;
		CvectorIter<T> bIter(b);
		return CvectorExp<ExprT,T>(ExprT(a,bIter));
};
template <class A,class T>
CvectorExp<CvectorBinExp<CvectorIter<T>,CvectorExp<A,T>,OpAdd<CvectorIter<T>,CvectorExp<A,T>,T>,T>,T> operator+(const Cvector<T>& a,const CvectorExp<A,T>& b)
{
		typedef CvectorBinExp<CvectorIter<T>,CvectorExp<A,T>,OpAdd<CvectorIter<T>,CvectorExp<A,T>,T>,T> ExprT;
		CvectorIter<T> aIter(a);
		return CvectorExp<ExprT,T>(ExprT(aIter,b));
};
template <class A,class B,class T>
CvectorExp<CvectorBinExp<CvectorExp<A,T>,CvectorExp<B,T>,OpAdd<CvectorExp<A,T>,CvectorExp<B,T>,T>,T>,T> operator+(const CvectorExp<A,T>& a,const CvectorExp<B,T>& b)
{
		typedef CvectorBinExp<CvectorExp<A,T>,CvectorExp<B,T>,OpAdd<CvectorExp<A,T>,CvectorExp<B,T>,T>,T> ExprT;
		return CvectorExp<ExprT,T>(ExprT(a,b));
};


// Substraction
template <class T>
CvectorExp<CvectorBinExp<CvectorIter<T>,CvectorIter<T>,OpSub<CvectorIter<T>,CvectorIter<T>,T>,T>,T> operator-(const Cvector<T>& a,const Cvector<T>& b)
{
    typedef CvectorBinExp<CvectorIter<T>,CvectorIter<T>,OpSub<CvectorIter<T>,CvectorIter<T>,T>,T> ExprT;
	CvectorIter<T> aIter(a),bIter(b);
    return CvectorExp<ExprT,T>(ExprT(aIter,bIter));
};


template <class A,class T>
CvectorExp<CvectorBinExp<CvectorExp<A,T>,CvectorIter<T>,OpSub<CvectorExp<A,T>,CvectorIter<T>,T>,T>,T> operator-(const CvectorExp<A,T>& a,const Cvector<T>& b)
{
		typedef CvectorBinExp<CvectorExp<A,T>,CvectorIter<T>,OpSub<CvectorExp<A,T>,CvectorIter<T>,T>,T> ExprT;
		CvectorIter<T> bIter(b);
		return CvectorExp<ExprT,T>(ExprT(a,bIter));
};
template <class A,class T>
CvectorExp<CvectorBinExp<CvectorIter<T>,CvectorExp<A,T>,OpSub<CvectorIter<T>,CvectorExp<A,T>,T>,T>,T> operator-(const Cvector<T>& a,const CvectorExp<A,T>& b)
{
		typedef CvectorBinExp<CvectorIter<T>,CvectorExp<A,T>,OpSub<CvectorIter<T>,CvectorExp<A,T>,T>,T> ExprT;
		CvectorIter<T> aIter(a);
		ExprT tmp(aIter,b);
		return CvectorExp<ExprT,T>(tmp);
};
template <class A,class B,class T>
CvectorExp<CvectorBinExp<CvectorExp<A,T>,CvectorExp<B,T>,OpSub<CvectorExp<A,T>,CvectorExp<B,T>,T>,T>,T> operator-(const CvectorExp<A,T>& a,const CvectorExp<B,T>& b)
{
		typedef CvectorBinExp<CvectorExp<A,T>,CvectorExp<B,T>,OpSub<CvectorExp<A,T>,CvectorExp<B,T>,T>,T> ExprT;
		return CvectorExp<ExprT,T>(ExprT(a,b));
};



// Multiplikation
template <class T>
CvectorExp<CvectorBinExp<CvectorIter<T>,CvectorIter<T>,OpMul<CvectorIter<T>,CvectorIter<T>,T>,T>,T> operator*(const Cvector<T>& a,const Cvector<T>& b)
{
    typedef CvectorBinExp<CvectorIter<T>,CvectorIter<T>,OpMul<CvectorIter<T>,CvectorIter<T>,T>,T> ExprT;
	CvectorIter<T> aIter(a),bIter(b);
    return CvectorExp<ExprT,T>(ExprT(aIter,bIter));
};





template <class A,class T>
CvectorExp<CvectorBinExp<CvectorExp<A,T>,CvectorIter<T>,OpMul<CvectorExp<A,T>,CvectorIter<T>,T>,T>,T> operator*(const CvectorExp<A,T>& a,const Cvector<T>& b)
{
		typedef CvectorBinExp<CvectorExp<A,T>,CvectorIter<T>,OpMul<CvectorExp<A,T>,CvectorIter<T>,T>,T> ExprT;
		CvectorIter<T> bIter(b);
		return CvectorExp<ExprT,T>(ExprT(a,bIter));
};
template <class A,class T>
CvectorExp<CvectorBinExp<CvectorIter<T>,CvectorExp<A,T>,OpMul<CvectorIter<T>,CvectorExp<A,T>,T>,T>,T> operator*(const Cvector<T>& a,const CvectorExp<A,T>& b)
{
		typedef CvectorBinExp<CvectorIter<T>,CvectorExp<A,T>,OpMul<CvectorIter<T>,CvectorExp<A,T>,T>,T> ExprT;
		CvectorIter<T> aIter(a);
		ExprT tmp(aIter,b);
		return CvectorExp<ExprT,T>(tmp);
};
template <class A,class B,class T>
CvectorExp<CvectorBinExp<CvectorExp<A,T>,CvectorExp<B,T>,OpMul<CvectorExp<A,T>,CvectorExp<B,T>,T>,T>,T> operator*(const CvectorExp<A,T>& a,const CvectorExp<B,T>& b)
{
		typedef CvectorBinExp<CvectorExp<A,T>,CvectorExp<B,T>,OpMul<CvectorExp<A,T>,CvectorExp<B,T>,T>,T> ExprT;
		return CvectorExp<ExprT,T>(ExprT(a,b));
};

//Mult with double
template <class T>
CvectorExp<CvectorBinExp<CbuiltinIter<double>,CvectorIter<T>,OpMul<CbuiltinIter<double>,CvectorIter<T>,T>,T>,T> operator*(double a,const Cvector<T>& b)
{
    typedef CvectorBinExp<CbuiltinIter<double>,CvectorIter<T>,OpMul<CbuiltinIter<double>,CvectorIter<T>,T>,T> ExprT;
	CvectorIter<T> bIter(b);
	CbuiltinIter<double> aIter(a);
    return CvectorExp<ExprT,T>(ExprT(aIter,bIter));
};

template <class T>
CvectorExp<CvectorBinExp<CvectorIter<T>,CbuiltinIter<double>,OpMul<CvectorIter<T>,CbuiltinIter<double>,T>,T>,T> operator*(const Cvector<T>& a,double b)
{
    typedef CvectorBinExp<CvectorIter<T>,CbuiltinIter<double>,OpMul<CvectorIter<T>,CbuiltinIter<double>,T>,T> ExprT;
	CvectorIter<T> aIter(a);
	CbuiltinIter<double> bIter(b);
    return CvectorExp<ExprT,T>(ExprT(aIter,bIter));
};

template <class A,class T>
CvectorExp<CvectorBinExp<CbuiltinIter<double>,CvectorExp<A,T>,OpMul<CbuiltinIter<double>,CvectorExp<A,T>,T>,T>,T> operator*(double a,const CvectorExp<A,T>& b)
{
    typedef CvectorBinExp<CbuiltinIter<double>,CvectorExp<A,T>,OpMul<CbuiltinIter<double>,CvectorExp<A,T>,T>,T> ExprT;
	CbuiltinIter<double> aIter(a);
    return CvectorExp<ExprT,T>(ExprT(aIter,b));
};

template <class A,class T>
CvectorExp<CvectorBinExp<CvectorExp<A,T>,CbuiltinIter<double>,OpMul<CvectorExp<A,T>,CbuiltinIter<double>,T>,T>,T> operator*(const CvectorExp<A,T>& a,double b)
{
    typedef CvectorBinExp<CvectorExp<A,T>,CbuiltinIter<double>,OpMul<CvectorExp<A,T>,CbuiltinIter<double>,T>,T> ExprT;
	CbuiltinIter<double> bIter(b);
    return CvectorExp<ExprT,T>(ExprT(a,bIter));
};


// Division
template <class T>
CvectorExp<CvectorBinExp<CvectorIter<T>,CvectorIter<T>,OpDiv<CvectorIter<T>,CvectorIter<T>,T>,T>,T> operator/(const Cvector<T>& a,const Cvector<T>& b)
{
    typedef CvectorBinExp<CvectorIter<T>,CvectorIter<T>,OpDiv<CvectorIter<T>,CvectorIter<T>,T>,T> ExprT;
	CvectorIter<T> aIter(a),bIter(b);
    return CvectorExp<ExprT,T>(ExprT(aIter,bIter));
};


template <class A,class T>
CvectorExp<CvectorBinExp<CvectorExp<A,T>,CvectorIter<T>,OpDiv<CvectorExp<A,T>,CvectorIter<T>,T>,T>,T> operator/(const CvectorExp<A,T>& a,const Cvector<T>& b)
{
		typedef CvectorBinExp<CvectorExp<A,T>,CvectorIter<T>,OpDiv<CvectorExp<A,T>,CvectorIter<T>,T>,T> ExprT;
		CvectorIter<T> bIter(b);
		return CvectorExp<ExprT,T>(ExprT(a,bIter));
};
template <class A,class T>
CvectorExp<CvectorBinExp<CvectorIter<T>,CvectorExp<A,T>,OpDiv<CvectorIter<T>,CvectorExp<A,T>,T>,T>,T> operator/(const Cvector<T>& a,const CvectorExp<A,T>& b)
{
		typedef CvectorBinExp<CvectorIter<T>,CvectorExp<A,T>,OpDiv<CvectorIter<T>,CvectorExp<A,T>,T>,T> ExprT;
		CvectorIter<T> aIter(a);
		ExprT tmp(aIter,b);
		return CvectorExp<ExprT,T>(tmp);
};

template <class A,class B,class T>
CvectorExp<CvectorBinExp<CvectorExp<A,T>,CvectorExp<B,T>,OpDiv<CvectorExp<A,T>,CvectorExp<B,T>,T>,T>,T> operator/(const CvectorExp<A,T>& a,const CvectorExp<B,T>& b)
{
		typedef CvectorBinExp<CvectorExp<A,T>,CvectorExp<B,T>,OpDiv<CvectorExp<A,T>,CvectorExp<B,T>,T>,T> ExprT;
		return CvectorExp<ExprT,T>(ExprT(a,b));
};


//division by double

template <class T>
CvectorExp<CvectorBinExp<CvectorIter<T>,CbuiltinIter<double>,OpDiv<CvectorIter<T>,CbuiltinIter<double>,T>,T>,T> operator/(const Cvector<T>& a,double b)
{
    typedef CvectorBinExp<CvectorIter<T>,CbuiltinIter<double>,OpDiv<CvectorIter<T>,CbuiltinIter<double>,T>,T> ExprT;
	CvectorIter<T> aIter(a);
	CbuiltinIter<double> bIter(b);
    return CvectorExp<ExprT,T>(ExprT(aIter,bIter));
};

template <class A,class T>
CvectorExp<CvectorBinExp<CvectorExp<A,T>,CbuiltinIter<double>,OpDiv<CvectorExp<A,T>,CbuiltinIter<double>,T>,T>,T> operator/(const CvectorExp<A,T>& a,double b)
{
    typedef CvectorBinExp<CvectorExp<A,T>,CbuiltinIter<double>,OpDiv<CvectorExp<A,T>,CbuiltinIter<double>,T>,T> ExprT;
	CbuiltinIter<double> bIter(b);
    return CvectorExp<ExprT,T>(ExprT(a,bIter));
};


// cross
template <class T>
CvectorExp<CvectorBinExp<CvectorIter<T>,CvectorIter<T>,OpCross<CvectorIter<T>,CvectorIter<T>,T>,T>,T> cross(const Cvector<T>& a,const Cvector<T>& b)
{
    typedef CvectorBinExp<CvectorIter<T>,CvectorIter<T>,OpCross<CvectorIter<T>,CvectorIter<T>,T>,T> ExprT;
	CvectorIter<T> aIter(a),bIter(b);
    return CvectorExp<ExprT,T>(ExprT(aIter,bIter));
};
template <class A,class T>
CvectorExp<CvectorBinExp<CvectorExp<A,T>,CvectorIter<T>,OpCross<CvectorExp<A,T>,CvectorIter<T>,T>,T>,T> cross(const CvectorExp<A,T>& a,const Cvector<T>& b)
{
    typedef CvectorBinExp<CvectorExp<A,T>,CvectorIter<T>,OpCross<CvectorExp<A,T>,CvectorIter<T>,T>,T> ExprT;
	CvectorIter<T> bIter(b);
    return CvectorExp<ExprT,T>(ExprT(a,bIter));
};
template <class A,class T>
CvectorExp<CvectorBinExp<CvectorIter<T>,CvectorExp<A,T>,OpCross<CvectorIter<T>,CvectorExp<A,T>,T>,T>,T> cross(const Cvector<T>& a,const CvectorExp<A,T>& b)
{
    typedef CvectorBinExp<CvectorIter<T>,CvectorExp<A,T>,OpCross<CvectorIter<T>,CvectorExp<A,T>,T>,T> ExprT;
	CvectorIter<T> aIter(a);
    return CvectorExp<ExprT,T>(ExprT(aIter,b));
};

template <class A,class B,class T>
CvectorExp<CvectorBinExp<CvectorExp<A,T>,CvectorExp<B,T>,OpCross<CvectorExp<A,T>,CvectorExp<B,T>,T>,T>,T> cross(const CvectorExp<A,T>& a,const CvectorExp<B,T>& b)
{
    typedef CvectorBinExp<CvectorExp<A,T>,CvectorExp<B,T>,OpCross<CvectorExp<A,T>,CvectorExp<B,T>,T>,T> ExprT;
    return CvectorExp<ExprT,T>(ExprT(a,b));
};


//dot
template <class A,class T>
T dot(const CvectorExp<A,T>& a,const CvectorExp<A,T>& b)
{
	Cvector<T> v(a.dim());
	v=a*b;
	return v.sum();
}
template <class T>
T dot(const Cvector<T>& a,const Cvector<T>& b)
{
	Cvector<T> v(a.dim());
	v=a*b;
	return v.sum();
}
template <class A,class T>
T dot(const CvectorExp<A,T>& a,const Cvector<T>& b)
{
	Cvector<T> v(a.dim());
	v=a*b;
	return v.sum();
}
template <class A,class T>
T dot(const Cvector<T>& a,const CvectorExp<A,T>& b)
{
	Cvector<T> v(a.dim());
	v=a*b;
	return v.sum();
}

//==
template <class A,class T>
bool operator==(const CvectorExp<A,T>& a,const CvectorExp<A,T>& b)
{
	int adim=a.dim();
	bool ret=true;
	if(adim!=b.dim())
		ret=false;
	Cvector<T> l(adim),r(adim);
	l=a;
	r=b;
	for(int i=0;i<adim;i++)
		if(l.get(i)!=r.get(i))
			ret=false;
	return ret;
}
template <class T>
bool operator==(const Cvector<T>& a,const Cvector<T>& b) 
{
	int adim=a.dim();
	bool ret=true;
	if(adim!=b.dim())
		ret=false;
	Cvector<T> l(adim),r(adim);
	l=a;
	r=b;
	for(int i=0;i<adim;i++)
		if(l.get(i)!=r.get(i))
			ret=false;
	return ret;
}
template <class A,class T>
bool operator==(const CvectorExp<A,T>& a,const Cvector<T>& b)
{
	int adim=a.dim();
	bool ret=true;
	if(adim!=b.dim())
		ret=false;
	Cvector<T> l(adim),r(adim);
	l=a;
	r=b;
	for(int i=0;i<adim;i++)
		if(l.get(i)!=r.get(i))
			ret=false;
	return ret;
}
template <class A,class T>
bool operator==(const Cvector<T>& a,const CvectorExp<A,T>& b)
{
	int adim=a.dim();
	bool ret=true;
	if(adim!=b.dim())
		ret=false;
	Cvector<T> l(adim),r(adim);
	l=a;
	r=b;
	for(int i=0;i<adim;i++)
		if(l.get(i)!=r.get(i))
			ret=false;
	return ret;
}


//unsigned angle
template <class A,class T>
double angle(const CvectorExp<A,T>& a,const CvectorExp<A,T>& b)
{
	Cvector<T> v;
	v=a*b;
	return acos(v.sum()/a.length()/b.length());
}
template <class T>
double angle(const Cvector<T>& a,const Cvector<T>& b)
{
	Cvector<T> v;
	v=a*b;
	return acos(v.sum()/a.length()/b.length());
}
template <class A,class T>
double angle(const CvectorExp<A,T>& a,const Cvector<T>& b)
{
	Cvector<T> v;
	v=a*b;
	return acos(v.sum()/a.length()/b.length());
}
template <class A,class T>
double angle(const Cvector<T>& a,const CvectorExp<A,T>& b)
{
	Cvector<T> v;
	v=a*b;
	return acos(v.sum()/a.length()/b.length());
}

//signed angle
template <class A,class T>
double sangle(const CvectorExp<A,T>& a,const CvectorExp<A,T>& b)
{
	Cvector<T> v,w;
	double sign;
	v=a*b;
	w=cross(a,b);
		sign=w[w.dim()-1]>=0?1:-1;
	return sign*acos(v.sum()/a.length()/b.length());
}
template <class T>
double sangle(const Cvector<T>& a,const Cvector<T>& b)
{
	Cvector<T> v,w;
	double sign;
	v=a*b;
	w=cross(a,b);
		sign=w[w.dim()-1]>=0?1:-1;
	return sign*acos(v.sum()/a.length()/b.length());
}
template <class A,class T>
double sangle(const CvectorExp<A,T>& a,const Cvector<T>& b)
{
	Cvector<T> v,w;
	double sign;
	v=a*b;
	w=cross(a,b);
		sign=w[w.dim()-1]>=0?1:-1;
	return sign*acos(v.sum()/a.length()/b.length());
}
template <class A,class T>
double sangle(const Cvector<T>& a,const CvectorExp<A,T>& b)
{
	Cvector<T> v,w;
	double sign;
	v=a*b;
	w=cross(a,b);
		sign=w[w.dim()-1]>=0?1:-1;
	return sign*acos(v.sum()/a.length()/b.length());
}

//output
template <class T>
ostream &operator<<(ostream &os,const Cvector<T>& v)
{
	os << "("<<v.get(0);
	int vard=v.dim();
	for(int i=1;i<vard;i++)
	{
		os <<','<<v.get(i);
	}
	os<<')';
	return os;
}

#endif

