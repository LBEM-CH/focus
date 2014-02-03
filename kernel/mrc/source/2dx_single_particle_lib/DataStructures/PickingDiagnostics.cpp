#include "PickingDiagnostics.hpp"

#include "../Utilities.hpp"

#include <omp.h>


SingleParticle2dx::DataStructures::PickingDiagnostics::PickingDiagnostics ()
{
	for (size_type i=0; i<5; i++)
	{
		m_data.push_back(size_type(0));
	}
}

SingleParticle2dx::DataStructures::PickingDiagnostics::~PickingDiagnostics ()
{}


void SingleParticle2dx::DataStructures::PickingDiagnostics::addGoodParticle()
{
	#pragma omp critical (inc_0)
	{	
		m_data[0]++;
	}
}


void SingleParticle2dx::DataStructures::PickingDiagnostics::addBadSdParticle()
{
	#pragma omp critical (inc_1)
	{
		m_data[1]++;
	}
}


void SingleParticle2dx::DataStructures::PickingDiagnostics::addTooLowSdParticle()
{
	#pragma omp critical (inc_2)
	{
		m_data[2]++;
	}
}


void SingleParticle2dx::DataStructures::PickingDiagnostics::addTooLowRelativeSdParticle()
{
	#pragma omp critical (inc_3)
	{
		m_data[3]++;
	}
}


void SingleParticle2dx::DataStructures::PickingDiagnostics::addFailingParticle()
{
	#pragma omp critical (inc_4)
	{
		m_data[4]++;
	}
}


SingleParticle2dx::DataStructures::PickingDiagnostics::size_type SingleParticle2dx::DataStructures::PickingDiagnostics::getNumberOfGoodParticles()
{
	return m_data[0];
}


void SingleParticle2dx::DataStructures::PickingDiagnostics::print(size_type level)
{
	SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("Picking Diagnosics:", level);
	SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("\t" + SingleParticle2dx::Utilities::StringFunctions::TtoString( m_data[0] + m_data[1] + m_data[2] + m_data[3] + m_data[4]) + "\tparticles picked", level);
	SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("\t" + SingleParticle2dx::Utilities::StringFunctions::TtoString( m_data[0] ) + "\tgood particles", level);
	SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("\t" + SingleParticle2dx::Utilities::StringFunctions::TtoString( m_data[1] ) + "\tdiscarded because of bad local sd deviation", level);
	SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("\t" + SingleParticle2dx::Utilities::StringFunctions::TtoString( m_data[2] ) + "\tdiscarded because of too low sd deviation", level);
	SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("\t" + SingleParticle2dx::Utilities::StringFunctions::TtoString( m_data[3] ) + "\tdiscarded because of too low relative sd deviation", level);
	SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("\t" + SingleParticle2dx::Utilities::StringFunctions::TtoString( m_data[4] ) + "\tdiscarded because of it failed the last test", level);
}

